{-# LANGUAGE ConstraintKinds, RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Development.Build.Compute (Compute, inputCompute, isInput) where

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

-- TODO: How do we express this in terms of Compute? Drop forall in Compute?
isInput :: ((k -> Proxy v) -> k -> Maybe (Proxy v)) -> k -> Bool
isInput compute = isNothing . compute (const Proxy)

--------------------------- Compute Functor: Collatz ---------------------------
-- Collatz sequence:
-- c[0] = n
-- c[k] = f(c[k - 1]) where
-- For example, if n = 12, the sequence is 3, 10, 5, 16, 8, 4, 2, 1, ...
data Collatz = Collatz Int

collatz :: Compute Functor Collatz Int
collatz get (Collatz k) | k <= 0    = Nothing
                        | otherwise = Just $ f <$> get (Collatz (k - 1))
  where
    f n | even n    = n `div` 2
        | otherwise = 3 * n + 1

-- A good demonstration of early cut-off:
-- * Compute Collatz sequence from n = 6: 6, 3, 10, 5, 16, 8, 4, 2, 1, ...
-- * Change n from 6 to 40 and rebuild: 40, 20, 10, 5, 16, 8, 4, 2, 1, ...
-- * The recomputation should be cut-off after 10.
------------------------ Compute Applicative: Fibonacci ------------------------
-- Generalised Fibonacci sequence:
-- f[0] = n
-- f[1] = m
-- f[k] = f[k - 1] + f[k - 2]
-- For example, with (n, m) = (0, 1) we get usual Fibonacci sequence, and if
-- (n, m) = (2, 1) we get Lucas sequence: 2, 1, 3, 4, 7, 11, 18, 29, 47, ...
data Fibonacci = Fibonacci Int

fibonacci :: Compute Applicative Fibonacci Int
fibonacci get (Fibonacci k) | k <= 1    = Nothing
                            | otherwise = Just $ (+) <$> get (Fibonacci (k - 1))
                                                     <*> get (Fibonacci (k - 2))

-- Fibonacci numbers are a classic example of memoization: a non-minimal build
-- system will take ages to compute f[100], doing O(f[100]) recursive calls.
-- The right approach is to build the dependency graph and execute computations
-- in the topological order.
--------------------------- Compute Monad: Ackermann ---------------------------
-- Ackermann function:
-- a[0, n] = n + 1
-- a[m, 0] = a[m - 1, 1]
-- a[m, n] = a[m - 1, a[m, n - 1]]
-- Formally, it has no inputs, but we return Nothing for negative inputs.
-- For example, a[m, 1] = 2, 3, 5, 13, 65535, ...

data Ackermann = Ackermann Int Int

ackermann :: Compute Monad Ackermann Int
ackermann get (Ackermann m n)
    | m < 0 || n < 0 = Nothing
    | m == 0    = Just $ return (n + 1)
    | n == 0    = Just $ get (Ackermann (m - 1) 1)
    | otherwise = Just $ do
        index <- get (Ackermann m (n - 1))
        get (Ackermann (m - 1) index)

-- Unlike Collatz and Fibonacci computations, the Ackermann computation cannot
-- be statically analysed for dependencies. We can only find the first dependency
-- statically (Ackermann m (n - 1)), but not the second one.

----------------------------- Spreadsheet examples -----------------------------
data Cell = A1 | A2 | B1 deriving Eq

add :: Compute Applicative Cell Integer
add fetch key | key /= B1 = Nothing
              | otherwise = Just $ (+) <$> fetch A1 <*> fetch A2


-- These type synonyms are not very useful, but enumerate all interesting cases.
type FunctorialCompute  k v = forall f. Functor     f => (k -> f v) -> k -> Maybe (f v)
type ApplicativeCompute k v = forall f. Applicative f => (k -> f v) -> k -> Maybe (f v)
type AlternativeCompute k v = forall f. Alternative f => (k -> f v) -> k -> Maybe (f v)
type MonadicCompute     k v = forall f. Monad       f => (k -> f v) -> k -> Maybe (f v)
type MonadPlusedCompute k v = forall f. MonadPlus   f => (k -> f v) -> k -> Maybe (f v)
