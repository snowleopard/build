{-# LANGUAGE ConstraintKinds, RankNTypes, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
module Build.Rebuilder (
    Rebuilder, PartialRebuilder, trying,
    perpetualRebuilder,
    modTimeRebuilder, Time, MakeInfo,
    approximationRebuilder, DependencyApproximation (..), ApproximationInfo, markDirty,
    vtRebuilder, stRebuilder, ctRebuilder, dctRebuilder
    ) where

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Map (Map)

import qualified Data.Map as Map

import Build.Store
import Build.Task
import Build.Task.Applicative (dependencies)
import Build.Task.Monad hiding (dependencies)
import Build.Trace

type Rebuilder c i k v = k -> v -> Task c k v -> Task (MonadState i) k v

type PartialRebuilder c i k v = k -> v -> Task c k v -> Task (MonadState i) k (Either k v)

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the task from the type of values @v@ to @Either e v@,
-- where the result @Left e@ indicates that the task failed, e.g. because of a
-- failed dependency lookup, and @Right v@ yeilds the value otherwise.
trying :: Task (MonadState i) k v -> Task (MonadState i) k (Either e v)
trying task fetch = runExceptT $ task (ExceptT . fetch)

-------------------------------- Always rebuild --------------------------------
perpetualRebuilder :: Rebuilder Monad () k v
perpetualRebuilder _key _value task = task

------------------------------------- Make -------------------------------------
type Time = Integer
type MakeInfo k = (Map k Time, Time)

modTimeRebuilder :: Ord k => Rebuilder Applicative (MakeInfo k) k v
modTimeRebuilder key value task fetch = do
    (modTime, now) <- get
    let dirty = case Map.lookup key modTime of
            Nothing -> True
            time -> any (\d -> Map.lookup d modTime > time) (dependencies task)
    if not dirty
    then return value
    else do
        put (Map.insert key now modTime, now + 1)
        task fetch

--------------------------- Dependency approximation ---------------------------
data DependencyApproximation k = Input | SubsetOf [k] | Unknown deriving (Eq, Show)

-- Initially, some keys are marked 'Dirty'.
-- After the build all non-input 'Dirty' keys are 'Rebuilt'.
-- Keys that were initially not in the map, will be either 'Skipped' or 'Rebuilt'
data Status = Dirty | Rebuilt | Skipped deriving (Eq, Show)

markDirty :: Ord k => [k] -> Map k Status
markDirty ks = Map.fromList [ (k, Dirty) | k <- ks ]

-- Nothing in the Map means k is not dirty and has not yet been processed
type ApproximationInfo k = (Map k Status, k -> DependencyApproximation k)

approximationRebuilder :: forall k v. Ord k => PartialRebuilder Monad (ApproximationInfo k) k v
approximationRebuilder key value task fetch = do
    (status, deps) <- get
    let is k s = Map.lookup k status == Just s
        dirty  = key `is` Dirty || case deps key of
                     Input       -> False -- Cannot happen
                     SubsetOf ks -> any (\k -> k `is` Dirty || k `is` Rebuilt) ks
                     Unknown     -> True
    if not dirty
    then do
        put (Map.insert key Skipped status, deps)
        return (Right value)
    else do
        put (Map.insert key Rebuilt status, deps)
        let newFetch k | k `is` Skipped || k `is` Rebuilt || deps k == Input = fetch k
                       | otherwise = return (Left k)
        trying task newFetch

------------------------------- Verifying traces -------------------------------
vtRebuilder :: (Eq k, Hashable v) => Rebuilder Monad (VT k v) k v
vtRebuilder key value task fetch = do
    vt <- get
    dirty <- not <$> verifyVT key value (fmap hash . fetch) vt
    if not dirty
    then return value
    else do
        (newValue, deps) <- trackM task fetch
        put =<< recordVT key newValue deps (fmap hash . fetch) =<< get
        return newValue

------------------------------- Version traces -------------------------------
stRebuilder :: (Eq k, Hashable v) => Rebuilder Monad (Step, ST k v) k v
stRebuilder key value task fetch = do
    dirty <- not <$> verifyST key value (void . fetch) (gets snd)
    if not dirty
    then return value
    else do
        (newValue, deps) <- trackM task fetch
        (step, st) <- get
        put . (step,) =<< recordST step key newValue deps st
        return newValue

------------------------------ Constructive traces -----------------------------
ctRebuilder :: (Eq k, Hashable v) => Rebuilder Monad (CT k v) k v
ctRebuilder key value task fetch = do
    ct <- get
    maybeCachedValue <- constructCT key value (fmap hash . fetch) ct
    case maybeCachedValue of
        Just cachedValue -> return cachedValue
        Nothing -> do
            (newValue, deps) <- trackM task fetch
            put =<< recordCT key newValue deps (fmap hash . fetch) =<< get
            return newValue

----------------------- Deterministic constructive traces ----------------------
dctRebuilder :: (Hashable k, Hashable v) => Rebuilder Monad (DCT k v) k v
dctRebuilder key _value task fetch = do
    dct <- get
    maybeCachedValue <- constructDCT key (fmap hash . fetch) dct
    case maybeCachedValue of
        Just cachedValue -> return cachedValue
        Nothing -> do
            (newValue, deps) <- trackM task fetch
            put =<< recordDCT key newValue deps (fmap hash . fetch) =<< get
            return newValue
