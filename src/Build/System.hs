{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Build.System (
    -- * Toy build systems
    dumb, busy, memo,

    -- * Applicative build systems
    make, ninja, bazel, buck,

    -- * Monadic build systems
    excel, shake, cloudShake, nix
    ) where

import Control.Monad.State
import Data.List
import Data.Semigroup

import Build
import Build.Algorithm
import Build.Store
import Build.Task.Monad
import Build.Trace

-- Not a correct build system
dumb :: Eq k => Build Monad i k v
dumb task key store = case compute task (flip getValue store) key of
    Nothing    -> store
    Just value -> putValue key value store

-- Not a minimal build system
busy :: forall k v. Eq k => Build Monad () k v
busy task key store = execState (fetch key) store
  where
    fetch :: k -> State (Store () k v) v
    fetch k = case task fetch k of
        Nothing  -> gets (getValue k)
        Just act -> do v <- act; modify (putValue k v); return v

-- Not a minimal build system, but never builds a key twice
memo :: Eq k => Build Monad () k v
memo = recursive $ \key _fetch act -> do
    (value, _deps) <- act
    modify $ \(store, t) -> (putValue key value store, t)

------------------------------------- Make -------------------------------------
type Time = Integer -- A negative time value means a key was never built
type MakeInfo k = (k -> Time, Time)

make :: forall k v. Ord k => Build Applicative (MakeInfo k) k v
make = topological process
  where
    process :: k -> [k] -> State (Store (MakeInfo k) k v) v -> State (Store (MakeInfo k) k v) ()
    process key deps act = do
        (modTime, now) <- gets getInfo
        let dirty = or [ modTime dep > modTime key | dep <- deps ]
        when (dirty || modTime key < 0) $ do
            v <- act
            let newModTime k = if k == key then now else modTime k
            modify $ putInfo (newModTime, now + 1) . putValue key v

------------------------------------- Ninja ------------------------------------
ninja :: (Ord k, Hashable v) => Build Applicative (VT k v) k v
ninja = topological $ \key deps act -> do
    store <- get
    let vt = getInfo store
    dirty <- not <$> verifyVT (return . flip getHash store) key vt
    when dirty $ do
        value <- act
        modify $ \s -> let newS = putValue key value s
                       in mapInfo (recordVT newS key deps <>) newS

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
bazel = topological $ \key deps act -> do
    store <- get
    let ct = getInfo store
    dirty <- not <$> verifyCT (return . flip getHash store) key ct
    when dirty $ do
        maybeValue <- constructCT (return . flip getHash store) key ct
        case maybeValue of
            Just value -> modify (putValue key value)
            Nothing -> do
                value <- act
                modify $ \s ->
                    let newS = putValue key value s
                    in mapInfo (recordCT newS key deps <>) newS

------------------------------------- Buck -------------------------------------
buck :: (Hashable k, Hashable v, Ord k) => Build Applicative (CTD k v) k v
buck = topological $ \key deps act -> do
    store <- get
    let ctd = getInfo store
    dirty <- not <$> verifyCTD (return . flip getHash store) key deps ctd
    when dirty $ do
        maybeValue <- constructCTD (return . flip getHash store) key deps ctd
        case maybeValue of
            Just value -> modify (putValue key value)
            Nothing -> do
                value <- act
                modify $ \s -> let newS = putValue key value s
                               in mapInfo (recordCTD newS key deps <>) newS

-------------------------------------- Nix -------------------------------------
nix :: (Hashable k, Hashable v, Ord k) => Build Monad (CTD k v) k v
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