{-# LANGUAGE RankNTypes #-}
module Development.Build.Compute (
    -- * Compute types
    Compute, IdentityCompute, FunctorialCompute, ApplicativeCompute,
    AlternativeCompute, MonadicCompute, MonadPlusedCompute
    ) where

import Control.Applicative
import Control.Monad

-- | Compute a value corresponding to a given key by performing necessary
-- lookups of the dependencies using the provided lookup function.
type Compute f k v i o = (k -> f v) -> i -> f o

type IdentityCompute    k v i o = forall f.                  Compute f k v i o
type FunctorialCompute  k v i o = forall f. Functor     f => Compute f k v i o
type ApplicativeCompute k v i o = forall f. Applicative f => Compute f k v i o
type AlternativeCompute k v i o = forall f. Alternative f => Compute f k v i o
type MonadicCompute     k v i o = forall m. Monad       m => Compute m k v i o
type MonadPlusedCompute k v i o = forall m. MonadPlus   m => Compute m k v i o
