{-# LANGUAGE RankNTypes #-}
module Development.Build.Compute.Functor (
    dependency, transitiveDependencies, acyclic
    ) where

import Data.Functor.Const

import Development.Build.Compute

dependency :: Compute Functor k v -> k -> k
dependency compute = getConst . compute Const

-- Compute Functor is always cyclic! They can't declare any keys as input
-- as they have no way to lift Nothing into the functor. They can do
--
-- compute get k = fmap (const Nothing) (get k)
--
-- But this still registers as a dependency on k even though the result is discarded.
transitiveDependencies :: Compute Functor k v -> k -> Maybe [k]
transitiveDependencies _ _ = Nothing

acyclic :: Compute Functor k v -> k -> Bool
acyclic _ _ = False
