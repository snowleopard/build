{-# LANGUAGE RankNTypes #-}
module Development.Build.Compute (
    Compute, inputCompute, FunctorialCompute, ApplicativeCompute,
    AlternativeCompute, MonadicCompute, MonadPlusedCompute,
    ) where

import Control.Applicative
import Control.Monad

-- | Compute a value corresponding to a given key by performing necessary
-- lookups of the dependencies using the provided lookup function.

type Compute f k v = (k -> f v) -> k -> f (Maybe v)

-- | The trivial computation that considers all keys as inputs.
inputCompute :: ApplicativeCompute k v
inputCompute _ _ = pure Nothing

type FunctorialCompute  k v = forall f. Functor     f => Compute f k v
type ApplicativeCompute k v = forall f. Applicative f => Compute f k v
type AlternativeCompute k v = forall f. Alternative f => Compute f k v
type MonadicCompute     k v = forall m. Monad       m => Compute m k v
type MonadPlusedCompute k v = forall m. MonadPlus   m => Compute m k v
