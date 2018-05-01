{-# LANGUAGE ConstraintKinds, RankNTypes, ScopedTypeVariables #-}
module Build.Strategy (
    Strategy, alwaysRebuildStrategy,
    makeStrategy, Time, MakeInfo,
    approximationStrategy, DependencyApproximation, ApproximationInfo,
    vtStrategy, ctStrategy, dctStrategy
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

type Strategy c i k v = k -> v -> Task c k v -> Task (MonadState i) k v

alwaysRebuildStrategy :: Strategy Monad () k v
alwaysRebuildStrategy _key _value task = task

------------------------------------- Make -------------------------------------
type Time = Integer -- A negative time value means a key was never built
type MakeInfo k = (Map k Time, Time)

makeStrategy :: Ord k => Strategy Applicative (MakeInfo k) k v
makeStrategy key value task fetch = do
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
data DependencyApproximation k = SubsetOf [k] | Unknown -- Add Exact [k]?

instance Ord k => Semigroup (DependencyApproximation k) where
    Unknown <> x = x
    x <> Unknown = x
    SubsetOf xs <> SubsetOf ys = SubsetOf (sort xs `intersect` sort ys)

instance Ord k => Monoid (DependencyApproximation k) where
    mempty  = Unknown
    mappend = (<>)

type ApproximationInfo k = (k -> Bool, k -> DependencyApproximation k)

approximationStrategy :: Ord k => Strategy Monad (ApproximationInfo k) k v
approximationStrategy key value task fetch = do
    (isDirty, deps) <- get
    let dirty = isDirty key || case deps key of SubsetOf ks -> any isDirty ks
                                                Unknown     -> True
    if not dirty
    then return value
    else do
        put (\k -> k == key || isDirty k, deps)
        task fetch

------------------------------- Verifying traces -------------------------------
vtStrategy :: (Eq k, Hashable v) => Strategy Monad (VT k v) k v
vtStrategy key value task fetch = do
    vt <- get
    dirty <- not <$> verifyVT key value (fmap hash . fetch) vt
    if not dirty
    then return value
    else do
        (newValue, deps) <- trackM task fetch
        put =<< recordVT key newValue deps (fmap hash . fetch) vt
        return newValue

------------------------------ Constructive traces -----------------------------
ctStrategy :: (Eq k, Hashable v) => Strategy Monad (CT k v) k v
ctStrategy key value task fetch = do
    ct <- get
    maybeCachedValue <- constructCT key value (fmap hash . fetch) ct
    case maybeCachedValue of
        Just cachedValue -> return cachedValue
        Nothing -> do
            (newValue, deps) <- trackM task fetch
            put =<< recordCT key newValue deps (fmap hash . fetch) ct
            return newValue

----------------------- Deterministic constructive traces ----------------------
dctStrategy :: (Hashable k, Hashable v) => Strategy Monad (DCT k v) k v
dctStrategy key _value task fetch = do
    dct <- get
    maybeCachedValue <- constructDCT key (fmap hash . fetch) dct
    case maybeCachedValue of
        Just cachedValue -> return cachedValue
        Nothing -> do
            (newValue, deps) <- trackM task fetch
            put =<< recordDCT key newValue deps (fmap hash . fetch) dct
            return newValue
