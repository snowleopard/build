{-# LANGUAGE ConstraintKinds, DeriveFunctor, FlexibleInstances, RankNTypes, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Build.Task (
    Task, Tasks, T (..), TT, compose, fetchIO, sprsh1, sprsh2, sprsh3
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail (MonadFail)

-- | Task a value corresponding to a given key by performing necessary
-- lookups of the dependencies using the provided lookup function. Returns
-- @Nothing@ to indicate that a given key is an input.
type Tasks c k v = forall f. c f => k -> Maybe ((k -> f v) -> f v)
type Task  c k v = forall f. c f =>             (k -> f v) -> f v

newtype T c k v a = T { run :: forall f. c f => (k -> f v) -> f a }

deriving instance Functor (T Functor     k v)
deriving instance Functor (T Applicative k v)
deriving instance Functor (T Alternative k v)
deriving instance Functor (T Monad       k v)
deriving instance Functor (T MonadPlus   k v)

instance Applicative (T Applicative k v) where
    pure x = T $ \_ -> pure x
    T f <*> T x = T $ \fetch -> f fetch <*> x fetch

instance Applicative (T Alternative k v) where
    pure x = T $ \_ -> pure x
    T f <*> T x = T $ \fetch -> f fetch <*> x fetch

instance Applicative (T Monad k v) where
    pure x = T $ \_ -> pure x
    T f <*> T x = T $ \fetch -> f fetch <*> x fetch

instance Applicative (T MonadPlus k v) where
    pure x = T $ \_ -> pure x
    T f <*> T x = T $ \fetch -> f fetch <*> x fetch

instance Monad (T Monad k v) where
    return x = T $ \_ -> return x
    T x >>= f = T $ \fetch -> x fetch >>= \a -> run (f a) fetch

instance Monad (T MonadPlus k v) where
    return x = T $ \_ -> return x
    T x >>= f = T $ \fetch -> x fetch >>= \a -> run (f a) fetch

instance Alternative (T Alternative k v) where
    empty = T $ \_ -> empty
    T x <|> T y = T $ \fetch -> x fetch <|> y fetch

instance Alternative (T MonadPlus k v) where
    empty = T $ \_ -> empty
    T x <|> T y = T $ \fetch -> x fetch <|> y fetch

instance MonadPlus (T MonadPlus k v) where
    mzero = empty
    mplus = (<|>)

type TT c k v = (k -> T c k v v) -> T c k v v

compose :: Tasks Monad k v -> Tasks Monad k v -> Tasks Monad k v
compose t1 t2 key = t1 key <|> t2 key

-- compose2 :: Tasks Monad k v -> Tasks Monad k v -> Tasks Monad k v
-- compose2 t1 t2 fetch key = t1 fetch key <|> t2 fetch key

-- type Tasks2 c k v = forall f. c f => (k -> f v) -> k -> Maybe (f v)

-- toTask :: Task2 Monad k v -> Task Monad k v
-- toTask task2 fetch key = ($fetch) <$> task2 key

-- fromTask :: Task Monad k v -> Task2 Monad k v
-- fromTask task key = runReaderT <$> task (\k -> ReaderT ($k)) key

--------------------------- Task Functor: Collatz ---------------------------
-- Collatz sequence:
-- c[0] = n
-- c[k] = f(c[k - 1]) where
-- For example, if n = 12, the sequence is 3, 10, 5, 16, 8, 4, 2, 1, ...
collatz :: Tasks Functor Integer Integer
collatz n | n <= 0    = Nothing
          | otherwise = Just $ \fetch -> f <$> fetch (n - 1)
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
    | n >= 2 = Just $ \fetch -> (+) <$> fetch (n-1) <*> fetch (n-2)
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
ackermann (n, m)
    | m < 0 || n < 0 = Nothing
    | m == 0    = Just $ const $ pure (n + 1)
    | n == 0    = Just $ \fetch -> fetch (m - 1, 1)
    | otherwise = Just $ \fetch -> do index <- fetch (m, n - 1)
                                      fetch (m - 1, index)

-- Unlike Collatz and Fibonacci computations, the Ackermann computation cannot
-- be statically analysed for dependencies. We can only find the first dependency
-- statically (Ackermann m (n - 1)), but not the second one.

----------------------------- Spreadsheet examples -----------------------------
sprsh1 :: Tasks Applicative String Integer
sprsh1 "B1" = Just $ \fetch -> ((+)  <$> fetch "A1" <*> fetch "A2")
sprsh1 "B2" = Just $ \fetch -> ((*2) <$> fetch "B1")
sprsh1 _    = Nothing

sprsh2 :: Tasks Monad String Integer
sprsh2 "B1" = Just $ \fetch -> do c1 <- fetch "C1"
                                  if c1 == 1 then fetch "B2" else fetch "A2"
sprsh2 "B2" = Just $ \fetch -> do c1 <- fetch "C1"
                                  if c1 == 1 then fetch "A1" else fetch "B1"
sprsh2 _ = Nothing

sprsh3 :: Tasks Alternative String Integer
sprsh3 "B1" = Just $ \fetch -> (+) <$> fetch "A1" <*> (pure 1 <|> pure 2)
sprsh3 _    = Nothing

sprsh4 :: Tasks MonadFail String Integer
sprsh4 "B1" = Just $ \fetch -> do
    a1 <- fetch "A1"
    a2 <- fetch "A2"
    if a2 == 0 then fail "division by 0" else return (a1 `div` a2)
sprsh4 _ = Nothing

indirect :: Tasks Monad String Integer
indirect key | key /= "B1" = Nothing
             | otherwise   = Just $ \fetch -> do c1 <- fetch "C1"
                                                 fetch ("A" ++ show c1)

staticIF :: Bool -> Tasks Applicative String Int
staticIF b "B1" = Just $ \fetch ->
    if b then fetch "A1" else (+) <$> fetch "A2" <*> fetch "A3"
staticIF _ _    = Nothing

fetchIO :: (Show k, Read v) => k -> IO v
fetchIO k = do putStr (show k ++ ": "); read <$> getLine

data Key = A Integer | B Integer | D Integer Integer

editDistance :: Tasks Monad Key Integer
editDistance (D i 0) = Just $ const $ pure i
editDistance (D 0 j) = Just $ const $ pure j
editDistance (D i j) = Just $ \fetch -> do
    ai <- fetch (A i)
    bj <- fetch (B j)
    if ai == bj
        then fetch (D (i - 1) (j - 1))
        else do
            insert  <- fetch (D  i      (j - 1))
            delete  <- fetch (D (i - 1)  j     )
            replace <- fetch (D (i - 1) (j - 1))
            return (1 + minimum [insert, delete, replace])
editDistance _ = Nothing

-- These type synonyms are not very useful, but enumerate all interesting cases.
type FunctorialTask  k v = forall f. Functor     f => (k -> f v) -> k -> Maybe (f v)
type ApplicativeTask k v = forall f. Applicative f => (k -> f v) -> k -> Maybe (f v)
type AlternativeTask k v = forall f. Alternative f => (k -> f v) -> k -> Maybe (f v)
type MonadicTask     k v = forall f. Monad       f => (k -> f v) -> k -> Maybe (f v)
type MonadPlusedTask k v = forall f. MonadPlus   f => (k -> f v) -> k -> Maybe (f v)
