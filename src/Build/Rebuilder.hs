{-# LANGUAGE ConstraintKinds, RankNTypes, ScopedTypeVariables, TupleSections #-}
module Build.Rebuilder (
    Rebuilder, perpetualRebuilder,
    modTimeRebuilder, Time, MakeInfo,
    approximationRebuilder, DependencyApproximation, ApproximationInfo,
    vtRebuilder, stRebuilder, ctRebuilder, dctRebuilder
    ) where

import Control.Monad.State
import Data.List
import Data.Map (Map)
import Data.Semigroup

import qualified Data.Map as Map

import Build.Store
import Build.Task
import Build.Task.Applicative (dependencies)
import Build.Task.Monad hiding (dependencies)
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
    let upToDate = case Map.lookup key modTime of
            Nothing -> False
            time -> all (\d -> Map.lookup d modTime < time) (dependencies task)
    if upToDate
    then return value
    else do
        put (Map.insert key now modTime, now + 1)
        task fetch

--------------------------- Dependency approximation ---------------------------
data DependencyApproximation k = SubsetOf [k] | Unknown -- Add Exact [k]?

instance Ord k => Semigroup (DependencyApproximation k) where
    Unknown <> x = x
    x <> Unknown = x
    SubsetOf xs <> SubsetOf ys = SubsetOf (sort xs `intersect` sort ys)

instance Ord k => Monoid (DependencyApproximation k) where
    mempty  = Unknown
    mappend = (<>)

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
    upToDate <- verifyVT key value (fmap hash . fetch)
    if upToDate
    then return value
    else do
        (newValue, deps) <- trackM task fetch
        recordVT key newValue deps (fmap hash . fetch)
        return newValue

------------------------------- Version traces -------------------------------
stRebuilder :: (Eq k, Hashable v) => Rebuilder Monad (ST k v) k v
stRebuilder key value task fetch = do
    upToDate <- verifyST key value (void . fetch)
    if upToDate
    then return value
    else do
        (newValue, deps) <- trackM task fetch
        recordST key newValue deps
        return newValue

------------------------------ Constructive traces -----------------------------
ctRebuilder :: (Eq k, Hashable v) => Rebuilder Monad (CT k v) k v
ctRebuilder key value task fetch = do
    maybeCachedValue <- constructCT key value (fmap hash . fetch)
    case maybeCachedValue of
        Just cachedValue -> return cachedValue
        Nothing -> do
            (newValue, deps) <- trackM task fetch
            recordCT key newValue deps (fmap hash . fetch)
            return newValue

----------------------- Deterministic constructive traces ----------------------
dctRebuilder :: (Hashable k, Hashable v) => Rebuilder Monad (DCT k v) k v
dctRebuilder key _value task fetch = do
    maybeCachedValue <- constructDCT key (fmap hash . fetch)
    case maybeCachedValue of
        Just cachedValue -> return cachedValue
        Nothing -> do
            (newValue, deps) <- trackM task fetch
            recordDCT key newValue deps (fmap hash . fetch)
            return newValue
