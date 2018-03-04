{-# LANGUAGE RankNTypes #-}
module Development.Build.Compute.Functor (
    dependency, transitiveDependencies, acyclic
    ) where

import Data.Functor.Const

import Development.Build.Compute

dependency :: (forall f. Functor f => Compute f k v) -> k -> k
dependency compute = getConst . compute Const

-- Compute Functor is always cyclic! They can't declare any keys as input
-- as they have no way to lift Nothing into the functor. They can do
--
-- compute get k = fmap (const Nothing) (get k)
--
-- But this still registers as a dependency on k even though the result is discarded.
transitiveDependencies :: (forall f. Functor f => Compute f k v) -> k -> Maybe [k]
transitiveDependencies _ _ = Nothing

acyclic :: (forall f. Functor f => Compute f k v) -> k -> Bool
acyclic _ _ = False
