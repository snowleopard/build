{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Build.Strategy (
    Strategy, alwaysRebuildStrategy,
    makeStrategy, Time, MakeInfo,
    approximationStrategy, DependencyApproximation, ApproximationInfo,
    tracingStrategyA, tracingStrategyM
    ) where

import Control.Monad.State
import Data.List
import Data.Semigroup

import Build.Store
import Build.Task
import Build.Task.Monad
import Build.Trace

import qualified Build.Task.Applicative as A

type Strategy c i k v = k -> v -> Task c k v -> Task (MonadState i) k v

alwaysRebuildStrategy :: Strategy Monad () k v
alwaysRebuildStrategy _key _value task = Task (run task)

------------------------------------- Make -------------------------------------
type Time = Integer -- A negative time value means a key was never built
type MakeInfo k = (k -> Time, Time)

makeStrategy :: Eq k => Strategy Applicative (MakeInfo k) k v
makeStrategy key value task = Task $ \fetch -> do
    (modTime, now) <- get
    let dirty = or [ modTime dep > modTime key | dep <- A.dependencies task ]
    if not (dirty || modTime key < 0)
    then return value
    else do
        let newModTime k = if k == key then now else modTime k
        put (newModTime, now + 1)
        run task fetch

--------------------------- Depencency approximation ---------------------------
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
approximationStrategy key value task = Task $ \fetch -> do
    (isDirty, deps) <- get
    let dirty = isDirty key || case deps key of SubsetOf ks -> any isDirty ks
                                                Unknown     -> True
    if not dirty
        then return value
        else do
            put (\k -> k == key || isDirty k, deps)
            run task fetch

------------------------------------ Traces ------------------------------------
tracingStrategyA :: (Hashable k, Hashable v, Semigroup (t k v), Trace t)
                 => Strategy Applicative (t k v) k v
tracingStrategyA key value task = Task $ \fetch -> do
    let deps = A.dependencies task
    t <- get
    dirty <- not <$> verify key value deps (fmap hash . fetch) t
    if not dirty
    then return value
    else do
        maybeCachedValue <- construct key deps (fmap hash . fetch) t
        case maybeCachedValue of
            Just cachedValue -> return cachedValue
            Nothing -> do
                newValue <- run task fetch
                newT <- record key newValue deps (fmap hash . fetch)
                modify (newT <>)
                return newValue

tracingStrategyM :: (Hashable k, Hashable v, Semigroup (t k v), Trace t)
                 => Strategy Monad (t k v) k v
tracingStrategyM key value task = Task $ \fetch -> do
    t <- get
    dirty <- not <$> verify key value [] (fmap hash . fetch) t
    if not dirty
    then return value
    else do
        maybeCachedValue <- construct key [] (fmap hash . fetch) t
        case maybeCachedValue of
            Just cachedValue -> return cachedValue
            Nothing -> do
                (newValue, deps) <- trackM task fetch
                newT <- record key newValue deps (fmap hash . fetch)
                modify (newT <>)
                return newValue
