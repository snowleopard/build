{-# LANGUAGE RankNTypes #-}
module Development.Build.Compute where

import Control.Applicative
import Control.Monad

-- | Compute a value corresponding to a given key by performing necessary
-- lookups of the dependencies using the provided lookup function. Returns
-- @Nothing@ to indicate that a given key is an input.
type Compute f k v = (k -> f v) -> k -> f (Maybe v)

-- Admittedly, these type synonyms are not very useful.
type FunctorialCompute  f k v = Functor     f => Compute f k v
type ApplicativeCompute f k v = Applicative f => Compute f k v
type AlternativeCompute f k v = Alternative f => Compute f k v
type MonadicCompute     f k v = Monad       f => Compute f k v
type MonadPlusedCompute f k v = MonadPlus   f => Compute f k v
