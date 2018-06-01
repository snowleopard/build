{-# LANGUAGE ConstraintKinds, RankNTypes, TupleSections #-}
module Build.Rebuilder (
    Rebuilder, perpetualRebuilder,
    modTimeRebuilder, Time, MakeInfo,
    approximationRebuilder, DependencyApproximation (..), ApproximationInfo,
    vtRebuilder, stRebuilder, ctRebuilder, dctRebuilder
    ) where

import Control.Monad.State
import Data.Map (Map)

import qualified Data.Map as Map

import Build.Store
import Build.Task
import Build.Task.Applicative
import Build.Task.Monad
import Build.Trace

type Rebuilder c i k v = k -> v -> Task c k v -> Task (MonadState i) k v

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
data DependencyApproximation k = SubsetOf [k] | Unknown

type ApproximationInfo k = (k -> Bool, k -> DependencyApproximation k)

approximationRebuilder :: Ord k => Rebuilder Monad (ApproximationInfo k) k v
approximationRebuilder key value task fetch = do
    (isDirty, deps) <- get
    let dirty = isDirty key || case deps key of SubsetOf ks -> any isDirty ks
                                                Unknown     -> True
    if not dirty
    then return value
    else do
        put (\k -> k == key || isDirty k, deps)
        task fetch

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
