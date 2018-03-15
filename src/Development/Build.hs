{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module Development.Build (
    -- * Build
    Build, dumb, busy, memo, make,

    -- * Algorithms
    topological,

    -- * MultiBuild
    MultiBuild, sequentialMultiBuild,

    -- * Properties
    correct, idempotent
    ) where

import Control.Monad.State

import Development.Build.Task
import Development.Build.Task.Applicative
import Development.Build.Task.Monad hiding (dependencies)
import Development.Build.Store
import Development.Build.Utilities

-- | A build system takes a 'Task', a key to build, some information from
-- the previous build @i@ (which can be missing if this is the first build),
-- and a key-value map @k -> v@, and computes information for the next build and
-- an updated key-value map. Note that we require @Eq k@ since without it one
-- has no way of updating the map.
type Build c i k v = Task c k v -> k -> Store i k v -> Store i k v

dumb :: Eq k => Build Monad i k v
dumb task key store = case compute task (getValue store) key of
    Nothing    -> store
    Just value -> putValue store key value

busy :: forall k v. Eq k => Build Monad () k v
busy task key store = execState (fetch key) store
  where
    fetch :: k -> State (Store () k v) v
    fetch k = case task fetch k of
        Nothing  -> do { s <- get; return (getValue s k) }
        Just act -> do { v <- act; modify (\s -> putValue s k v); return v }

memo :: forall k v. Eq k => Build Monad () k v
memo task key store = fst $ execState (fetch key) (store, [])
  where
    fetch :: k -> State (Store () k v, [k]) v
    fetch k = case task fetch k of
        Nothing  -> do { s <- fst <$> get; return (getValue s k) }
        Just act -> do
            built <- snd <$> get
            when (k `notElem` built) $ do
                v <- act
                modify $ \(s, built) -> (putValue s k v, k : built)
            s <- fst <$> get
            return (getValue s k)

topological :: Eq k
            => (k -> [k] -> State (Store i k v) v -> State (Store i k v) ())
            -> Build Applicative i k v
topological step task key = execState $ forM_ chain $ \k -> do
    let fetch k = do { store <- get; return (getValue store k) }
    case task fetch k of
        Nothing  -> return ()
        Just act -> step k (deps k) act
  where
    deps  = dependencies task
    chain = topSort deps (closure deps key)

type Time = Integer
type MakeInfo k = (k -> Time, Time)

make :: Eq k => Build Applicative (MakeInfo k) k v
make = topological $ \key deps act -> do
    (modTime, now) <- getInfo <$> get
    let dirty = or [ modTime dep > modTime key | dep <- deps ]
    when dirty $ do
        v <- act
        let newModTime k = if k == key then now else modTime k
        modify $ \s -> putInfo (putValue s key v) (newModTime, now + 1)

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
    in forall $ \k -> getValue store2 k == getValue store3 k
