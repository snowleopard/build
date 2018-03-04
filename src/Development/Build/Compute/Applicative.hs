{-# LANGUAGE RankNTypes #-}
module Development.Build.Compute.Applicative (
    inputCompute, pureCompute, dependencies, transitiveDependencies, acyclic,
    runPartial
    ) where

import Control.Applicative
import Data.Maybe

import Development.Build.Compute
import Development.Build.Utilities

-- | The trivial compute that considers all keys as inputs.
inputCompute :: Applicative f => Compute f k v
inputCompute _ _ = pure Nothing

-- | Lift a pure function to an applicative compute.
pureCompute :: Applicative f => (k -> v) -> Compute f k v
pureCompute f _ = pure . Just . f

-- TODO: Does this always terminate? It's not obvious!
dependencies :: (forall f. Applicative f => Compute f k v) -> k -> [k]
dependencies compute = getConst . compute (Const . return)

transitiveDependencies :: Eq k => (forall f. Applicative f => Compute f k v) -> k -> Maybe [k]
transitiveDependencies compute = reach (dependencies compute)

acyclic :: Eq k => (forall f. Applicative f => Compute f k v) -> k -> Bool
acyclic compute = isJust . transitiveDependencies compute

-- | Run a compute with a partial lookup function. The result @Left k@ indicates
-- that the compute failed due to a missing dependency @k@. Otherwise, the
-- result @Right (Just v)@ yields the computed value, and @Right Nothing@ is
-- returned if the given key is an input.
runPartial :: Applicative f => (forall g. Applicative g => Compute g k v)
                            -> (k -> f (Maybe v)) -> k -> f (Either k (Maybe v))
runPartial compute partialGet = runEitherT . compute get
  where
    get k = EitherT $ maybe (Left k) Right <$> partialGet k
