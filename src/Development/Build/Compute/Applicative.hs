{-# LANGUAGE RankNTypes #-}
module Development.Build.Compute.Applicative (
    pureCompute, dependencies, transitiveDependencies, acyclic, debugPartial,
    partial, exceptional
    ) where

import Control.Applicative
import Data.Functor.Compose
import Data.Maybe

import Development.Build.Compute
import Development.Build.Utilities

-- | Lift a pure function to an applicative compute.
pureCompute :: (k -> v) -> Compute Applicative k v
pureCompute store _ = Just . pure . store

-- TODO: Does this always terminate? It's not obvious!
dependencies :: Compute Applicative k v -> k -> [k]
dependencies compute = maybe [] getConst . compute (\k -> Const [k])

transitiveDependencies :: Eq k => Compute Applicative k v -> k -> Maybe [k]
transitiveDependencies compute = reach (dependencies compute)

acyclic :: Eq k => Compute Applicative k v -> k -> Bool
acyclic compute = isJust . transitiveDependencies compute

-- | Run a compute with a partial lookup function. The result @Left k@ indicates
-- that the compute failed due to a missing dependency @k@. Otherwise, the
-- result @Right (Just v)@ yields the computed value, and @Right Nothing@ is
-- returned if the given key is an input.
debugPartial :: Applicative f => Compute Applicative k v
                            -> (k -> f (Maybe v)) -> k -> Maybe (f (Either k v))
debugPartial compute store = fmap getCompose . compute (Compose . fetch)
  where
    fetch k = maybe (Left k) Right <$> store k

-- | Convert a compute with a total lookup function @k -> m v@ into a compute
-- with a partial lookup function @k -> m (Maybe v)@. This essentially lifts the
-- compute from the type of values @v@ to @Maybe v@, where the result @Nothing@
-- indicates that the compute failed because of a missing dependency.
-- Use 'debugPartial' if you need to know which dependency was missing.
partial :: Compute Applicative k v -> Compute Applicative k (Maybe v)
partial compute fetch = fmap getCompose . compute (Compose . fetch)

-- | Convert a compute with a total lookup function @k -> m v@ into a compute
-- with a lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the compute from the type of values @v@ to @Either e v@,
-- where the result @Left e@ indicates that the compute failed because of a
-- failed dependency lookup, and @Right v@ yeilds the value otherwise.
exceptional :: Compute Applicative k v -> Compute Applicative k (Either e v)
exceptional compute fetch = fmap getCompose . compute (Compose . fetch)
