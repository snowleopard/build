{-# LANGUAGE ConstraintKinds, RankNTypes #-}
module Development.Build.Compute (Compute, inputCompute) where

-- | Compute a value corresponding to a given key by performing necessary
-- lookups of the dependencies using the provided lookup function. Returns
-- @Nothing@ to indicate that a given key is an input.
type Compute c k v = forall f. c f => (k -> f v) -> k -> f (Maybe v)

-- | The trivial computation that considers all keys as inputs.
inputCompute :: Compute Applicative k v
inputCompute _ _ = pure Nothing
