{-# LANGUAGE ConstraintKinds, RankNTypes, GADTs #-}
module Examples where

import Build.Task
import Control.Applicative
import Control.Monad.Fail (MonadFail)
import Control.Monad.State.Class
import Data.Map (Map)

import qualified Data.Map as Map

-- | A useful fetch for experimenting with build systems in interactive GHC.
fetchIO :: (Show k, Read v) => k -> IO v
fetchIO k = do putStr (show k ++ ": "); read <$> getLine

--------------------------- Task Functor: Collatz ---------------------------

-- Collatz sequence, starting with an initial value n = c[0].
-- For example, if n = 6, the sequence is 6, 3, 10, 5, 16, 8, 4, 2, 1, ...
collatz :: Tasks Functor Integer Integer
collatz n | n <= 0    = Nothing
          | otherwise = Just $ Task $ \fetch -> f <$> fetch (n - 1)
  where
    f k | even k    = k `div` 2
        | otherwise = 3 * k + 1

-- A good demonstration of early cut-off:
-- * Task Collatz sequence from n = 6: 6, 3, 10, 5, 16, 8, 4, 2, 1, ...
-- * Change n from 6 to 40 and rebuild: 40, 20, 10, 5, 16, 8, 4, 2, 1, ...
-- * The recomputation should be cut-off after 10.

------------------------ Task Applicative: Fibonacci ------------------------

-- Generalised Fibonacci sequence:
-- f[0] = n
-- f[1] = m
-- f[k] = f[k - 1] + f[k - 2]
-- For example, with (n, m) = (0, 1) we get usual Fibonacci sequence, and if
-- (n, m) = (2, 1) we get Lucas sequence: 2, 1, 3, 4, 7, 11, 18, 29, 47, ...
fibonacci :: Tasks Applicative Integer Integer
fibonacci n
    | n >= 2 = Just $ Task $ \fetch -> (+) <$> fetch (n-1) <*> fetch (n-2)
    | otherwise = Nothing

-- Fibonacci numbers are a classic example of memoization: a non-minimal build
-- system will take ages to compute f[100], doing O(f[100]) recursive calls.
-- The right approach is to build the dependency graph and execute computations
-- in the topological order.

--------------------------- Task Monad: Ackermann ---------------------------

-- Ackermann function:
-- a[0, n] = n + 1
-- a[m, 0] = a[m - 1, 1]
-- a[m, n] = a[m - 1, a[m, n - 1]]
-- Formally, it has no inputs, but we return Nothing for negative inputs.
-- For example, a[m, 1] = 2, 3, 5, 13, 65535, ...
ackermann :: Tasks Monad (Integer, Integer) Integer
ackermann (m, n)
    | m < 0 || n < 0 = Nothing
    | m == 0    = Just $ Task $ const $ pure (n + 1)
    | n == 0    = Just $ Task $ \fetch -> fetch (m - 1, 1)
    | otherwise = Just $ Task $ \fetch -> do index <- fetch (m, n - 1)
                                             fetch (m - 1, index)

-- A cloud version of the Ackermann task that uses a cache to store known values
-- of the Ackermann function.
type Cache = Map (Integer, Integer) Integer

cloudAckermann :: Tasks (MonadState Cache) (Integer, Integer) Integer
cloudAckermann (m, n)
    | m < 0 || n < 0 = Nothing
    | m == 0    = Just $ Task $ const $ pure (n + 1)
    | n == 0    = Just $ Task $ \fetch -> fetch (m - 1, 1)
    | otherwise = Just $ Task $ \fetch -> do
        cache <- get
        case Map.lookup (m, n) cache of
            Nothing -> do index <- fetch (m, n - 1)
                          value <- fetch (m - 1, index)
                          modify (Map.insert (m, n) value)
                          return value
            Just value -> return value

-- Unlike Collatz and Fibonacci computations, the Ackermann computation cannot
-- be statically analysed for dependencies. We can only find the first dependency
-- statically (Ackermann m (n - 1)), but not the second one.

----------------------------- Spreadsheet examples -----------------------------

sprsh1 :: Tasks Applicative String Integer
sprsh1 "B1" = Just $ Task $ \fetch -> ((+)  <$> fetch "A1" <*> fetch "A2")
sprsh1 "B2" = Just $ Task $ \fetch -> ((*2) <$> fetch "B1")
sprsh1 _    = Nothing

sprsh2 :: Tasks Monad String Integer
sprsh2 "B1" = Just $ Task $ \fetch -> do
    c1 <- fetch "C1"
    if c1 == 1 then fetch "B2" else fetch "A2"
sprsh2 "B2" = Just $ Task $ \fetch -> do
    c1 <- fetch "C1"
    if c1 == 1 then fetch "A1" else fetch "B1"
sprsh2 _ = Nothing

sprsh5 :: Tasks Monad String String
sprsh5 "B1" = Just $ Task $ \fetch -> do
    formula <- fetch "B1-formula"
    evalFormula fetch formula
  where
    evalFormula = undefined
sprsh5 _ = Nothing

sprsh3 :: Tasks Alternative String Integer
sprsh3 "B1" = Just $ Task $ \fetch -> (+) <$> fetch "A1" <*> (pure 1 <|> pure 2)
sprsh3 _    = Nothing

sprsh4 :: Tasks MonadFail String Integer
sprsh4 "B1" = Just $ Task $ \fetch -> do
    a1 <- fetch "A1"
    a2 <- fetch "A2"
    if a2 == 0 then fail "division by 0" else return (a1 `div` a2)
sprsh4 _ = Nothing

indirect :: Tasks Monad String Integer
indirect key | key /= "B1" = Nothing
             | otherwise   = Just $ Task $ \fetch -> do c1 <- fetch "C1"
                                                        fetch ("A" ++ show c1)

staticIF :: Bool -> Tasks Applicative String Int
staticIF b "B1" = Just $ Task $ \fetch ->
    if b then fetch "A1" else (+) <$> fetch "A2" <*> fetch "A3"
staticIF _ _    = Nothing

-------------------------- Dynamic programming example -------------------------

data K = A Int | B Int | C Int Int deriving Eq

editDistance :: Tasks Monad K Int
editDistance (C i 0) = Just $ Task $ const $ pure i
editDistance (C 0 j) = Just $ Task $ const $ pure j
editDistance (C i j) = Just $ Task $ \fetch -> do
    ai <- fetch (A i)
    bj <- fetch (B j)
    if ai == bj
        then fetch (C (i - 1) (j - 1))
        else do
            insert  <- fetch (C  i      (j - 1))
            delete  <- fetch (C (i - 1)  j     )
            replace <- fetch (C (i - 1) (j - 1))
            return (1 + minimum [insert, delete, replace])
editDistance _ = Nothing
