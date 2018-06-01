{-# LANGUAGE ConstraintKinds, RankNTypes, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
module Build.Rebuilder (
    Rebuilder, PartialRebuilder, try,
    perpetualRebuilder,
    modTimeRebuilder, Time, MakeInfo,
    approximationRebuilder, Status (..), DependencyApproximation (..), ApproximationInfo,
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

-- Partial rebuilders return a task that fails if one if its dependencies is
-- dirty or has not yet been processed. Used by restarting build schedulers.
type PartialRebuilder c i k v = k -> v -> Task c k v -> Task (MonadState i) k (Either k v)

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the task from the type of values @v@ to @Either e v@,
-- where the result @Left e@ indicates that the task failed, e.g. because of a
-- failed dependency lookup, and @Right v@ yeilds the value otherwise.
try :: Task (MonadState i) k v -> Task (MonadState i) k (Either e v)
try task fetch = runExceptT $ task (ExceptT . fetch)

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
-- | Approximation of task dependencies:
-- * A subset of keys, e.g. @SubsefOf [x, y, z]@ for @IF(x > 0, y, z)@.
-- * Unknown dependencies, e.g. for @INDIRECT@ spreadsheet references.
data DependencyApproximation k = SubsetOf [k] | Unknown deriving (Eq, Show)

-- | The status of a key during the build. Before the build, some input keys are
-- marked 'NewInput', e.g. when the user edits a number in a spreadsheet, or
-- 'Dirty' when the user edits a formula thus changing the task. After the build
-- all 'Dirty' keys become 'Rebuilt'. Keys that initially have no status, are
-- either 'Skipped' (if they are up to date) or 'Rebuilt'.
data Status = NewInput | Dirty | Rebuilt | Skipped deriving (Eq, Show)

-- | We store the status and dependency approximation for each key. @Nothing@
-- in the status map means the key is not dirty and has not yet been processed.
type ApproximationInfo k = (Map k Status, k -> DependencyApproximation k)

approximationRebuilder :: Ord k => PartialRebuilder Monad (ApproximationInfo k) k v
approximationRebuilder key value task fetch = do
    (status, deps) <- get
    let is k s = Map.lookup k status == Just s
        dirty  = key `is` Dirty || case deps key of
                     SubsetOf ks -> any (\k -> k `is` NewInput || k `is` Dirty || k `is` Rebuilt) ks
                     Unknown     -> True
    if not dirty
    then do
        put (Map.insert key Skipped status, deps)
        return (Right value)
    else do
        put (Map.insert key Rebuilt status, deps)
        let newFetch k | k `is` NewInput || k `is` Skipped || k `is` Rebuilt = fetch k
                       | otherwise = return (Left k)
        try task newFetch

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

-- ctPartialRebuilder :: (Eq k, Hashable v) => PartialRebuilder Monad (Map k Status, CT k v) k v
-- ctPartialRebuilder key value task fetch = do
--     (status, ct) <- get
--     let is k s = Map.lookup k status == Just s
--         dirty  = key `is` Dirty || case deps key of
--                      Input       -> False -- This cannot happen
--                      SubsetOf ks -> any (\k -> k `is` Dirty || k `is` Rebuilt) ks
--                      Unknown     -> True
--     if not dirty
--     then do
--         put (Map.insert key Skipped status, deps)
--         return (Right value)
--     else do
--         put (Map.insert key Rebuilt status, deps)
--         let newFetch k | k `is` Skipped || k `is` Rebuilt || deps k == Input = fetch k
--                        | otherwise = return (Left k)
--         try task newFetch

    -- ct <- get
    -- maybeCachedValue <- constructCT key value (fmap hash . fetch) ct
    -- case maybeCachedValue of
    --     Just cachedValue -> return cachedValue
    --     Nothing -> do
    --         (newValue, deps) <- trackM task fetch
    --         put =<< recordCT key newValue deps (fmap hash . fetch) =<< get
    --         return newValue
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
