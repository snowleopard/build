{-# LANGUAGE RankNTypes #-}
module Development.Build.Compute.Identity (
    IdentityCompute, identityCompute, dependency, isInput
    ) where

type IdentityCompute k v = forall f. (k -> f v) -> k -> f v

-- The only possible implementation
identityCompute :: IdentityCompute k v
identityCompute = id

dependency :: IdentityCompute k v -> k -> k
dependency _ = id

isInput :: IdentityCompute k v -> k -> Bool
isInput _ _ = True
