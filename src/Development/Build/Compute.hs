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
type Compute f k v = (k -> f v) -> k -> f v

type IdentityCompute    k v = forall f.                  Compute f k v
type FunctorialCompute  k v = forall f. Functor     f => Compute f k v
type ApplicativeCompute k v = forall f. Applicative f => Compute f k v
type AlternativeCompute k v = forall f. Alternative f => Compute f k v
type MonadicCompute     k v = forall m. Monad       m => Compute m k v
type MonadPlusedCompute k v = forall m. MonadPlus   m => Compute m k v
