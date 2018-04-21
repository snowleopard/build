{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Build.Task.Depend (toDepend, Depend (..), toDepends, Depends (..)) where

import Build.Task

----------------------------- Free Task Applicative ----------------------------
data Depend k v r = Depend [k] ([v] -> r)
    deriving Functor

instance Applicative (Depend k v) where
    pure v = Depend [] (\[] -> v)
    Depend d1 f1 <*> Depend d2 f2 = Depend (d1++d2) $ \vs -> let (v1,v2) = splitAt (length d1) vs in f1 v1 $ f2 v2

toDepend :: Task Applicative k v -> Depend k v v
toDepend (Task f) = f $ \k -> Depend [k] $ \[v] -> v

-------------------------------- Free Task Monad -------------------------------
data Depends k v r = Depends [k] ([v] -> Depends k v r)
                   | Done r
    deriving Functor

instance Applicative (Depends k v) where
    pure = return
    f1 <*> f2 = f2 >>= \v -> ($ v) <$> f1

instance Monad (Depends k v) where
    return = Done
    Done x >>= f = f x
    Depends ds op >>= f = Depends ds $ \vs -> f =<< op vs

toDepends :: Task Monad k v -> Depends k v v
toDepends (Task f) = f $ \k -> Depends [k] $ \[v] -> Done v
