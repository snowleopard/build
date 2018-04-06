{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Build.System (
    -- * Toy build systems
    dumb, busy, memo,

    -- * Applicative build systems
    make, ninja, bazel, -- buck,

    -- * Monadic build systems
    excel, shake, cloudShake, nix
    ) where

import Control.Monad.State
import Data.List
import Data.Semigroup

import Build
import Build.Algorithm
import Build.Store
import Build.Task
import Build.Task.Monad
import Build.Trace

import qualified Build.Task.Applicative as A

-- Not a correct build system
dumb :: Eq k => Build Monad i k v
dumb tasks key store = case tasks key of
    Nothing   -> store
    Just task -> putValue key (compute task (flip getValue store)) store

-- Not a minimal build system
busy :: forall k v. Eq k => Build Monad () k v
busy tasks key store = execState (fetch key) store
  where
    fetch :: k -> State (Store () k v) v
    fetch k = case tasks k of
        Nothing   -> gets (getValue k)
        Just task -> do v <- run task fetch; modify (putValue k v); return v

-- Not a minimal build system, but never builds a key twice
memo :: Eq k => Build Monad () k v
memo = recursive $ \key _fetch act -> do
    (value, _deps) <- act
    modify $ \(store, t) -> (putValue key value store, t)

------------------------------------- Make -------------------------------------
type Time = Integer -- A negative time value means a key was never built
type MakeInfo k = (k -> Time, Time)

make :: forall k v. Ord k => Build Applicative (MakeInfo k) k v
make = topological transformer
  where
    transformer :: k -> v -> Task Applicative k v -> Task (MonadState (MakeInfo k)) k v
    transformer key currentValue task = Task $ \fetch -> do
        (modTime, now) <- get
        let dirty = or [ modTime dep > modTime key | dep <- A.dependencies task ]
        if dirty || modTime key < 0
        then do
            let newModTime k = if k == key then now else modTime k
            put (newModTime, now + 1)
            run task fetch
        else
            return currentValue

------------------------------------- Ninja ------------------------------------
ninja :: (Ord k, Hashable v) => Build Applicative (VT k v) k v
ninja = topological $ \key currentValue task -> Task $ \fetch -> do
    vt <- get
    dirty <- not <$> verifyVT (fmap hash . fetch) key vt
    if dirty
    then do
        value <- run task fetch
        newVT <- recordVT' key value (A.dependencies task) fetch
        modify (newVT <>)
        return value
    else
        return currentValue

------------------------------------- Excel ------------------------------------
data DependencyApproximation k = SubsetOf [k] | Unknown -- Add Exact [k]?

instance Ord k => Semigroup (DependencyApproximation k) where
    Unknown <> x = x
    x <> Unknown = x
    SubsetOf xs <> SubsetOf ys = SubsetOf (sort xs `intersect` sort ys)

instance Ord k => Monoid (DependencyApproximation k) where
    mempty  = Unknown
    mappend = (<>)

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

------------------------------------- Shake ------------------------------------
shake :: (Eq k, Hashable v) => Build Monad (VT k v) k v
shake = recursive $ \key fetch act -> do
    vt <- gets (getInfo . fst)
    dirty <- not <$> verifyVT (fmap hash . fetch) key vt
    when dirty $ do
        (value, deps) <- act
        modify $ \(s, t) ->
            let newS = putValue key value s
            in (mapInfo (recordVT newS key deps <>) newS, t)

---------------------------------- Cloud Shake ---------------------------------
cloudShake :: (Eq k, Hashable v) => Build Monad (CT k v) k v
cloudShake = recursive $ \key fetch act -> do
    ct <- gets (getInfo . fst)
    dirty <- not <$> verifyCT (fmap hash . fetch) key ct
    when dirty $ do
        maybeValue <- constructCT (fmap hash . fetch) key ct
        case maybeValue of
            Just value -> modify $ \(s, t) -> (putValue key value s, t)
            Nothing -> do
                (value, deps) <- act
                modify $ \(s, t) ->
                    let newS = putValue key value s
                    in (mapInfo (recordCT newS key deps <>) newS, t)

------------------------------------- Bazel ------------------------------------
bazel :: (Ord k, Hashable v) => Build Applicative (CT k v) k v
bazel = topological $ \key currentValue task -> Task $ \fetch -> do
    ct <- get
    dirty <- not <$> verifyCT (fmap hash . fetch) key ct
    if dirty
    then do
        maybeValue <- constructCT (fmap hash . fetch) key ct
        case maybeValue of
            Just value -> return value
            Nothing -> do
                value <- run task fetch
                newCT <- recordCT' key value (A.dependencies task) fetch
                modify (newCT <>)
                return value
    else
        return currentValue

------------------------------------- Buck -------------------------------------
-- buck :: (Hashable k, Hashable v) => Build Applicative (CTD k v) k v
-- buck = topological $ \key deps act -> do
--     store <- get
--     let ctd = getInfo store
--     dirty <- not <$> verifyCTD (return . flip getHash store) key deps ctd
--     when dirty $ do
--         maybeValue <- constructCTD (return . flip getHash store) key deps ctd
--         case maybeValue of
--             Just value -> modify (putValue key value)
--             Nothing -> do
--                 value <- act
--                 modify $ \s -> let newS = putValue key value s
--                                in mapInfo (recordCTD newS key deps <>) newS

-------------------------------------- Nix -------------------------------------
nix :: (Hashable k, Hashable v) => Build Monad (CTD k v) k v
nix = recursive $ \key fetch act -> do
    ctd <- gets (getInfo . fst)
    let deps = [] -- Here is the tricky part: we need to store this in CTD
    dirty <- not <$> verifyCTD (fmap hash . fetch) key deps ctd
    when dirty $ do
        maybeValue <- constructCTD (fmap hash . fetch) key deps ctd
        case maybeValue of
            Just value -> modify $ \(s, t) -> (putValue key value s, t)
            Nothing -> do
                (value, deps) <- act
                modify $ \(s, t) ->
                    let newS = putValue key value s
                    in (mapInfo (recordCTD newS key deps <>) newS, t)