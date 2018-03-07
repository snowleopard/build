{-# LANGUAGE ConstraintKinds, RankNTypes #-}
module Development.Build.Compute where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Proxy

-- | Compute a value corresponding to a given key by performing necessary
-- lookups of the dependencies using the provided lookup function. Returns
-- @Nothing@ to indicate that a given key is an input.
type Compute c k v = forall f. c f => (k -> f v) -> k -> Maybe (f v)

-- | The trivial compute that considers all keys as inputs.
inputCompute :: (k -> f v) -> k -> Maybe (f v)
inputCompute _ _ = Nothing

-- TODO: How do we say it works for any possible constraint?
isInput :: Compute MonadPlus k v -> k -> Bool
isInput compute = isNothing . compute (const Proxy)

----------------------------------- Examples -----------------------------------
-- Collatz sequence:
-- a[0] = n
-- a[k] = f(a[k - 1]) where
-- For example, if n = 12, one gets the sequence 12, 6, 3, 10, 5, 16, 8, 4, 2, 1.

data Collatz = Collatz Int

collatz :: Compute Functor Collatz Int
collatz get (Collatz k) | k == 0    = Nothing
                        | otherwise = Just $ f <$> get (Collatz (k - 1))
  where
    f n | even n    = n `div` 2
        | otherwise = 3 * n + 1

-- These type synonyms are not very useful, but enumerate all interesting cases.
type FunctorialCompute  k v = forall f. Functor     f => (k -> f v) -> k -> Maybe (f v)
type ApplicativeCompute k v = forall f. Applicative f => (k -> f v) -> k -> Maybe (f v)
type AlternativeCompute k v = forall f. Alternative f => (k -> f v) -> k -> Maybe (f v)
type MonadicCompute     k v = forall f. Monad       f => (k -> f v) -> k -> Maybe (f v)
type MonadPlusedCompute k v = forall f. MonadPlus   f => (k -> f v) -> k -> Maybe (f v)
