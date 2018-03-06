{-# LANGUAGE RankNTypes #-}
module Development.Build.Compute.Applicative (
    pureCompute, dependencies, transitiveDependencies, acyclic, runPartial
    ) where

import Control.Applicative
import Data.Maybe

import Development.Build.Compute
import Development.Build.Utilities

-- | Lift a pure function to an applicative compute.
pureCompute :: (k -> v) -> Compute Applicative k v
pureCompute f _ = Just . pure . f

-- TODO: Does this always terminate? It's not obvious!
dependencies :: Compute Applicative k v -> k -> [k]
dependencies compute = getConst . sequenceA . compute (Const . return)

transitiveDependencies :: Eq k => Compute Applicative k v -> k -> Maybe [k]
transitiveDependencies compute = reach (dependencies compute)

acyclic :: Eq k => Compute Applicative k v -> k -> Bool
acyclic compute = isJust . transitiveDependencies compute

-- | Run a compute with a partial lookup function. The result @Left k@ indicates
-- that the compute failed due to a missing dependency @k@. Otherwise, the
-- result @Right (Just v)@ yields the computed value, and @Right Nothing@ is
-- returned if the given key is an input.
runPartial :: Applicative f => Compute Applicative k v
                            -> (k -> f (Maybe v)) -> k -> Maybe (f (Either k v))
runPartial compute partialGet = fmap runEitherT . compute get
  where
    get k = EitherT $ maybe (Left k) Right <$> partialGet k
