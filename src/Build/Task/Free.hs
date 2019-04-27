{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | The free description of tasks.
module Build.Task.Free(
    Depend(..), toDepend, fromDepend,
    Depends(..), toDepends, fromDepends
    ) where

import Build.Task


data Depend k v r = Depend [k] ([v] -> r)
    deriving Functor

instance Applicative (Depend k v) where
    pure v = Depend [] (\[] -> v)
    Depend d1 f1 <*> Depend d2 f2 = Depend (d1++d2) $ \vs -> let (v1,v2) = splitAt (length d1) vs in f1 v1 $ f2 v2


data Depends k v r = Depends k (v -> Depends k v r)
                   | Done r
    deriving Functor

instance Applicative (Depends k v) where
    pure = return
    f1 <*> f2 = f2 >>= \v -> ($ v) <$> f1

instance Monad (Depends k v) where
    return = Done
    Done x >>= f = f x
    Depends ds op >>= f = Depends ds $ \vs -> f =<< op vs

getDepend :: k -> Depend k v v
getDepend k = Depend [k] $ \[v] -> v

toDepend :: Task Applicative k v -> Depend k v v
toDepend (Task f) = f getDepend

getDepends :: k -> Depends k v v
getDepends k = Depends k Done

toDepends :: Task Monad k v -> Depends k v v
toDepends (Task f) = f getDepends

fromDepend :: Depend k v v -> Task Applicative k v
fromDepend (Depend ds f) = Task $ \fetch -> f <$> traverse fetch ds

fromDepends :: Depends k v v -> Task Monad k v
fromDepends x = Task $ \fetch -> f fetch x
    where
        f _ (Done v) = return v
        f fetch (Depends d op) = f fetch . op =<< fetch d
