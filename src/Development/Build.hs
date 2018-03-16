{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Development.Build (
    -- * Build
    Build, dumb, busy, memo, make, excel, shake,

    -- * Algorithms
    topological, reordering, recursive,

    -- * MultiBuild
    MultiBuild, sequentialMultiBuild,

    -- * Properties
    correct, idempotent
    ) where

import Control.Monad.State
import Data.Set (Set)
import Data.Map (Map)
import Control.Monad.Extra

import Development.Build.Task
import Development.Build.Task.Applicative hiding (exceptional)
import Development.Build.Task.Monad hiding (dependencies)
import Development.Build.Store
import Development.Build.Utilities

import qualified Data.Set as Set
import qualified Data.Map as Map

-- | A build system takes a 'Task', a key to build, some information from
-- the previous build @i@ (which can be missing if this is the first build),
-- and a key-value map @k -> v@, and computes information for the next build and
-- an updated key-value map. Note that we require @Eq k@ since without it one
-- has no way of updating the map.
type Build c i k v = Task c k v -> k -> Store i k v -> Store i k v

dumb :: Eq k => Build Monad i k v
dumb task key store = case compute task (\k -> getValue k store) key of
    Nothing    -> store
    Just value -> putValue key value store

busy :: forall k v. Eq k => Build Monad () k v
busy task key store = execState (fetch key) store
  where
    fetch :: k -> State (Store () k v) v
    fetch k = case task fetch k of
        Nothing  -> gets (getValue k)
        Just act -> do v <- act; modify (putValue k v); return v

memo :: forall k v. Eq k => Build Monad () k v
memo task key store = fst $ execState (fetch key) (store, [])
  where
    fetch :: k -> State (Store () k v, [k]) v
    fetch k = case task fetch k of
        Nothing  -> gets (getValue k . fst)
        Just act -> do
            built <- snd <$> get
            when (k `notElem` built) $ do
                v <- act
                modify $ \(s, built) -> (putValue k v s, k : built)
            gets (getValue k . fst)

topological :: Eq k
            => (k -> [k] -> State (Store i k v) v -> State (Store i k v) ())
            -> Build Applicative i k v
topological process task key = execState $ forM_ chain $ \k -> do
    let fetch k = gets (getValue k)
    case task fetch k of
        Nothing  -> return ()
        Just act -> process k (deps k) act
  where
    deps  = dependencies task
    chain = topSort deps (closure deps key)

type Time = Integer
type MakeInfo k = (k -> Time, Time)

make :: Eq k => Build Applicative (MakeInfo k) k v
make = topological process
  where
    process key deps act = do
        (modTime, now) <- getInfo <$> get
        let dirty = or [ modTime dep > modTime key | dep <- deps ]
        when dirty $ do
            v <- act
            let newModTime k = if k == key then now else modTime k
            modify $ putInfo (newModTime, now + 1) . putValue key v

-- Add constructor Exact [k]?
data DependencyApproximation k = SubsetOf [k] | Unknown

data Result k v = MissingDependency k | Result v [k]

try :: forall m k v. Monad m => Task Monad k v -> (k -> m (Maybe v))
                           -> k -> Maybe (m (Result k v))
try task partialFetch = fmap (fmap toResult) . trackExceptions task partialFetch
  where
    toResult (Left k       ) = MissingDependency k
    toResult (Right (v, ks)) = Result v ks

type CalcChain k = [k]

reordering :: forall i k v. Ord k
            => (k -> State (Store i k v) (Result k v) -> State (Store i k v) (Maybe (Result k v)))
            -> Build Monad (i, CalcChain k) k v
reordering step task key = execState $ do
    chain    <- snd . getInfo <$> get
    newChain <- go Set.empty $ chain ++ [key | key `notElem` chain]
    modify $ \s -> putInfo (fst (getInfo s), newChain) s
  where
    go :: Set k -> CalcChain k -> State (Store (i, [k]) k v) (CalcChain k)
    go _    []     = return []
    go done (k:ks) = do
        case try task fetch k of
            Nothing -> (k :) <$> go (Set.insert k done) ks
            Just act -> do
                store <- get
                let (res, newStore) = runState (step k act) (mapInfo fst store)
                put $ mapInfo (,[]) newStore
                case res of
                    Just (MissingDependency d) -> go done $ [ d | d `notElem` ks ] ++ ks ++ [k]
                    _                          -> (k :) <$> go (Set.insert k done) ks
      where
        fetch :: k -> State (Store i k v) (Maybe v)
        fetch k | k `Set.member` done = gets (Just . getValue k)
                | otherwise           = return Nothing

type ExcelInfo k = ((k -> Bool, k -> DependencyApproximation k), CalcChain k)

excel :: Ord k => Build Monad (ExcelInfo k) k v
excel = reordering process
  where
    process key act = do
        (dirty, deps) <- gets getInfo
        let rebuild = dirty key || case deps key of SubsetOf ks -> any dirty ks
                                                    Unknown     -> True
        if not rebuild
            then return Nothing
            else do
                result <- act
                case result of
                    MissingDependency _ -> return ()
                    Result v _dynamicDependencies -> do
                        let newDirty k = if k == key then True else dirty k
                        modify $ putInfo (newDirty, deps) . putValue key v
                return (Just result)

type MultiBuild c i k v = Task c k v -> [k] -> Store i k v -> Store i k v

sequentialMultiBuild :: Build Monad i k v -> MultiBuild Monad i k v
sequentialMultiBuild build task outputs store = case outputs of
    []     -> store
    (k:ks) -> sequentialMultiBuild build task ks (build task k store)

-- | Given a @build@ and @task@, check that for any key-value map describing
-- the contents of a store @before@ the build system is executed to build a list
-- of @outputs@, the map @after@ the build is a correct build outcome.
-- Specifically, there must exist a @magic@ key-value map, such that:
-- * @before@, @after@ and @magic@ agree on the values of all inputs.
-- * @after@ and @magic@ agree on the values of all outputs.
-- * @magic@ is 'consistent' with the @task@.
-- We assume that @task@ is acyclic. If it is not, the function returns @True@.
correct :: (Eq k, Eq v) => Build Monad i k v -> Task Monad k v -> Bool
correct build task = forall $ \(key, store) ->
    correctBuild task store (build task key store) key

-- TODO: Switch to getHash
-- | Check that a build system is /idempotent/, i.e. running it once or twice in
-- a row leads to the same resulting 'Store'.
idempotent :: Eq v => Build Monad i k v -> Task Monad k v -> Bool
idempotent build task = forall $ \(key, store1) ->
    let store2 = build task key store1
        store3 = build task key store2
    in forall $ \k -> getValue k store2 == getValue k store3


data Trace k v = Trace
    { key          :: k
    , depends :: [(k, Hash v)]
    , result       :: Hash v }


-- Determine whether a trace is relevant to the current state
traceMatch :: (Monad m, Eq k) => (k -> Hash v -> m Bool) -> k -> [Trace k v] -> m [Hash v]
traceMatch check key ts = mapMaybeM f ts
    where f (Trace k dkv v) = do
                b <- return (key == k) &&^ allM (uncurry check) dkv
                return $ if b then Just v else Nothing

-- Recursive dependency strategy
recursive
    :: Eq k => (k -> (k -> State (Store i k v, [k]) v) -> State (Store i k v, [k]) (v, [k]) -> State (Store i k v, [k]) ())
    -> Build Monad i k v
recursive process task key store = fst $ execState (ensure key) (store, [])
    where
        ensure key = do
            let fetch k = do ensure k; gets (getValue k . fst)
            done <- gets snd
            when (key `notElem` done) $ do
                modify $ \(s, done) -> (s, key:done)
                case trackM task fetch key of
                    Nothing -> return ()
                    Just act -> process key fetch act

-- Shake build system
shake :: (Eq k, Hashable v) => Build Monad [Trace k v] k v
shake = recursive $ \key fetch act -> do
    traces <- gets (getInfo . fst)
    poss <- traceMatch (\k v -> (==) v . hash <$> fetch k) key traces
    current <- gets (getHash key . fst)
    when (current `notElem` poss) $ do
        (v, ds) <- act
        modify $ \(s, done) ->
            let t = Trace key [(d, getHash d s) | d <- ds] (getHash key s)
            in (putInfo (t : getInfo s) (putValue key v s), done)

data Traces k v = Traces
    { traces :: [Trace k v]
    , contents  :: Map (Hash v) v }

bazel :: (Eq k, Hashable v) => Build Applicative (Traces k v) k v
bazel = topological $ \key ds act -> do
    s <- get
    let Traces traces contents = getInfo s
    poss <- traceMatch (\k v -> return $ getHash k s == v) key traces
    if null poss then do
        v <- act
        modify $ \s ->
            let t = Trace key [(d, getHash d s) | d <- ds] (getHash key s)
                ts = Traces (t : traces) (Map.insert (hash v) v contents)
            in putInfo ts (putValue key v s)
    else do
        when (getHash key s `notElem` poss) $
            modify $ putValue key (contents Map.! head poss)
