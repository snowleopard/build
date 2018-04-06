{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Build.System (
    -- * Toy build systems
    dumb, busy, memo,

    -- * Applicative build systems
    make, ninja, bazel, buck,

    -- * Monadic build systems
    excel, shake, cloudShake --, nix
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
memo = recursive $ \_key _value task -> Task (run task)

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
        if not (dirty || modTime key < 0)
        then return currentValue
        else do
            let newModTime k = if k == key then now else modTime k
            put (newModTime, now + 1)
            run task fetch

------------------------------------- Ninja ------------------------------------
ninja :: (Ord k, Hashable v) => Build Applicative (VT k v) k v
ninja = topological $ \key currentValue task -> Task $ \fetch -> do
    vt <- get
    dirty <- not <$> verifyVT key currentValue (fmap hash . fetch) vt
    if not dirty
    then return currentValue
    else do
        value <- run task fetch
        newVT <- recordVT key value (A.dependencies task) (fmap hash . fetch)
        modify (newVT <>)
        return value

------------------------------------- Excel ------------------------------------
data DependencyApproximation k = SubsetOf [k] | Unknown -- Add Exact [k]?

instance Ord k => Semigroup (DependencyApproximation k) where
    Unknown <> x = x
    x <> Unknown = x
    SubsetOf xs <> SubsetOf ys = SubsetOf (sort xs `intersect` sort ys)

instance Ord k => Monoid (DependencyApproximation k) where
    mempty  = Unknown
    mappend = (<>)

type ExcelInfo k = ((k -> Bool, k -> DependencyApproximation k), Chain k)

excel :: Ord k => Build Monad (ExcelInfo k) k v
excel = reordering $ \key currentValue task -> Task $ \fetch -> do
    (isDirty, deps) <- get
    let dirty = isDirty key || case deps key of SubsetOf ks -> any isDirty ks
                                                Unknown     -> True
    if not dirty
        then return currentValue
        else do
            put (\k -> k == key || isDirty k, deps)
            run task fetch

------------------------------------- Shake ------------------------------------
shake :: (Eq k, Hashable v) => Build Monad (VT k v) k v
shake = recursive $ \key currentValue task -> Task $ \fetch -> do
    vt <- get
    dirty <- not <$> verifyVT key currentValue (fmap hash . fetch) vt
    if not dirty
    then return currentValue
    else do
        (value, deps) <- trackM fetch task
        newVT <- recordVT key value deps (fmap hash . fetch)
        modify (newVT <>)
        return value

---------------------------------- Cloud Shake ---------------------------------
-- Currently broken: loops forever
cloudShake :: (Eq k, Hashable v) => Build Monad (CT k v) k v
cloudShake = recursive $ \key currentValue task -> Task $ \fetch -> do
    ct <- get
    dirty <- not <$> verifyCT key currentValue (fmap hash . fetch) ct
    if not dirty
    then return currentValue
    else do
        maybeValue <- constructCT key (fmap hash . fetch) ct
        case maybeValue of
            Just value -> return value
            Nothing -> do
                (value, deps) <- trackM fetch task
                newCT <- recordCT key value deps (fmap hash . fetch)
                modify (newCT <>)
                return value

------------------------------------- Bazel ------------------------------------
bazel :: (Ord k, Hashable v) => Build Applicative (CT k v) k v
bazel = topological $ \key currentValue task -> Task $ \fetch -> do
    ct <- get
    dirty <- not <$> verifyCT key currentValue (fmap hash . fetch) ct
    if not dirty
    then return currentValue
    else do
        maybeValue <- constructCT key (fmap hash . fetch) ct
        case maybeValue of
            Just value -> return value
            Nothing -> do
                value <- run task fetch
                newCT <- recordCT key value (A.dependencies task) (fmap hash . fetch)
                modify (newCT <>)
                return value

------------------------------------- Buck -------------------------------------
buck :: (Hashable k, Hashable v) => Build Applicative (DCT k v) k v
buck = topological $ \key currentValue task -> Task $ \fetch -> do
    dct <- get
    dirty <- not <$> verifyDCT key (A.dependencies task) (fmap hash . fetch) dct
    if not dirty
    then return currentValue
    else do
        maybeValue <- constructDCT key (A.dependencies task) (fmap hash . fetch) dct
        case maybeValue of
            Just value -> return value
            Nothing -> do
                value <- run task fetch
                newDCT <- recordDCT key value (A.dependencies task) (fmap hash . fetch)
                modify (newDCT <>)
                return value

-------------------------------------- Nix -------------------------------------
-- nix :: (Hashable k, Hashable v) => Build Monad (DCT k v) k v
-- nix = recursive $ \key fetch act -> do
--     ctd <- gets (getInfo . fst)
--     let deps = [] -- Here is the tricky part: we need to store this in CTD
--     dirty <- not <$> verifyCTD (fmap hash . fetch) key deps ctd
--     when dirty $ do
--         maybeValue <- constructCTD (fmap hash . fetch) key deps ctd
--         case maybeValue of
--             Just value -> modify $ \(s, t) -> (putValue key value s, t)
--             Nothing -> do
--                 (value, deps) <- act
--                 modify $ \(s, t) ->
--                     let newS = putValue key value s
--                     in (mapInfo (recordCTD newS key deps <>) newS, t)
