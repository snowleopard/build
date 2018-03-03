{-# LANGUAGE RankNTypes #-}
module Development.Build.Compute (Compute, inputCompute, runPartial) where

import Control.Monad.Trans
import Control.Monad.Trans.Except

-- | Compute a value corresponding to a given key by performing necessary
-- lookups of the dependencies using the provided lookup function. Returns
-- @Nothing@ to indicate that a given key is an input.
type Compute f k v = (k -> f v) -> k -> f (Maybe v)

-- | The trivial computation that considers all keys as inputs.
inputCompute :: Applicative f => Compute f k v
inputCompute _ _ = pure Nothing

-- | Run a given compute with a partial lookup function. The result @Left k@
-- indicates that the compute failed due to a missing dependency @k@. Otherwise,
-- the result @Right Nothing@ indicates that the given key is an input, and
-- @Right (Just v)@ yields the computed value.
runPartial :: Monad m => (forall f. Compute f k v)
                      -> (k -> m (Maybe v)) -> k -> m (Either k (Maybe v))
runPartial compute partialGet = runExceptT . compute get
  where
    get k = do
        maybeValue <- lift (partialGet k)
        case maybeValue of
            Nothing    -> throwE k
            Just value -> return value

