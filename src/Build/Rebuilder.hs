{-# LANGUAGE TupleSections #-}

-- | Rebuilders take care of deciding whether a key needs to be rebuild and
-- running the corresponding task if need be.
module Build.Rebuilder (
    Rebuilder, adaptRebuilder, perpetualRebuilder,
    modTimeRebuilder, Time, MakeInfo,
    dirtyBitRebuilder,
    approximateRebuilder, ApproximateDependencies, ApproximationInfo,
    vtRebuilder, stRebuilder, ctRebuilder, dctRebuilder
    ) where

import Control.Monad.State
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Build.Store
import Build.Task
import Build.Task.Applicative
import Build.Task.Monad
import Build.Trace

-- | Given a key-value pair and the corresponding task, a rebuilder returns a
-- new task that has access to the build information and can use it to skip
-- rebuilding a key if it is up to date.
type Rebuilder c i k v = k -> v -> Task c k v -> Task (MonadState i) k v

-- | Get an applicative rebuilder out of a monadic one.
adaptRebuilder :: Rebuilder Monad i k v -> Rebuilder Applicative i k v
adaptRebuilder rebuilder key value task = rebuilder key value $ Task $ run task

-- | Always rebuilds the key.
perpetualRebuilder :: Rebuilder Monad () k v
perpetualRebuilder _key _value task = Task $ run task

------------------------------------- Make -------------------------------------
type Time = Integer
type MakeInfo k = (Time, Map k Time)

-- | This rebuilder uses modification time to decide whether a key is dirty and
-- needs to be rebuilt. Used by Make.
modTimeRebuilder :: Ord k => Rebuilder Applicative (MakeInfo k) k v
modTimeRebuilder key value task = Task $ \fetch -> do
    (now, modTimes) <- get
    let dirty = case Map.lookup key modTimes of
            Nothing -> True
            time -> any (\d -> Map.lookup d modTimes > time) (dependencies task)
    if not dirty
    then return value
    else do
        put (now + 1, Map.insert key now modTimes)
        run task fetch

----------------------------------- Dirty bit ----------------------------------
-- | If the key is dirty, rebuild it. Used by Excel.
dirtyBitRebuilder :: Rebuilder Monad (k -> Bool) k v
dirtyBitRebuilder key value task = Task $ \fetch -> do
    isDirty <- get
    if isDirty key then run task fetch else return value

--------------------------- Approximate dependencies ---------------------------
-- | If there is an entry for a key, it is an conservative approximation of its
-- dependencies. Otherwise, we have no reasonable approximation and assume the
-- key is always dirty (e.g. it uses an INDIRECT reference).
type ApproximateDependencies k = Map k [k]

-- | A set of dirty keys and information about dependencies.
type ApproximationInfo k = (Set k, ApproximateDependencies k)

-- | This rebuilders uses approximate dependencies to decide whether a key
-- needs to be rebuilt.
approximateRebuilder :: (Ord k, Eq v) => Rebuilder Monad (ApproximationInfo k) k v
approximateRebuilder key value task = Task $ \fetch -> do
    (dirtyKeys, deps) <- get
    let dirty = key `Set.member` dirtyKeys ||
                case Map.lookup key deps of Nothing -> True
                                            Just ks -> any (`Set.member` dirtyKeys) ks
    if not dirty
    then return value
    else do
        newValue <- run task fetch
        when (value /= newValue) $ put (Set.insert key dirtyKeys, deps)
        return newValue

------------------------------- Verifying traces -------------------------------
-- | This rebuilder relies on verifying traces.
vtRebuilder :: (Eq k, Hashable v) => Rebuilder Monad (VT k v) k v
vtRebuilder key value task = Task $ \fetch -> do
    upToDate <- verifyVT key value (fmap hash . fetch) =<< get
    if upToDate
    then return value
    else do
        (newValue, deps) <- trackM task fetch
        put =<< recordVT key newValue deps (fmap hash . fetch) =<< get
        return newValue

------------------------------ Constructive traces -----------------------------
-- | This rebuilder relies on constructive traces.
ctRebuilder :: (Eq k, Hashable v) => Rebuilder Monad (CT k v) k v
ctRebuilder key value task = Task $ \fetch -> do
    maybeCachedValue <- constructCT key value (fmap hash . fetch) =<< get
    case maybeCachedValue of
        Just cachedValue -> return cachedValue
        Nothing -> do
            (newValue, deps) <- trackM task fetch
            put =<< recordCT key newValue deps (fmap hash . fetch) =<< get
            return newValue

----------------------- Deterministic constructive traces ----------------------
-- | This rebuilder relies on deterministic constructive traces.
dctRebuilder :: (Hashable k, Hashable v) => Rebuilder Monad (DCT k v) k v
dctRebuilder key _value task = Task $ \fetch -> do
    maybeCachedValue <- constructDCT key (fmap hash . fetch) =<< get
    case maybeCachedValue of
        Just cachedValue -> return cachedValue
        Nothing -> do
            (newValue, deps) <- trackM task fetch
            put =<< recordDCT key newValue deps (fmap hash . fetch) =<< get
            return newValue

------------------------------- Version traces -------------------------------
-- | This rebuilder relies on version/step traces.
stRebuilder :: (Eq k, Hashable v) => Rebuilder Monad (Step, ST k v) k v
stRebuilder key value task = Task $ \fetch -> do
    upToDate <- verifyST key value (void . fetch) (gets snd)
    if upToDate
    then return value
    else do
        (newValue, deps) <- trackM task fetch
        (step, st) <- get
        put . (step,) =<< recordST step key newValue deps st
        return newValue
