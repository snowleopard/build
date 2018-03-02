module Development.Build.Compute (Compute, inputCompute) where

-- | Compute a value corresponding to a given key by performing necessary
-- lookups of the dependencies using the provided lookup function. Returns
-- @Nothing@ to indicate that a given key is an input.
type Compute f k v = (k -> f v) -> k -> f (Maybe v)

-- | The trivial computation that considers all keys as inputs.
inputCompute :: Applicative f => Compute f k v
inputCompute _ _ = pure Nothing
