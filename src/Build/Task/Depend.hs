{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | The \"free\" structures for dependencies, providing either an applicative
-- interface (for 'Depend') or a monadic interface (for 'Depends'). By passing
-- them to a suitable 'Task' you can reconstruct all necessary dependencies.
module Build.Task.Depend (toDepend, Depend (..), toDepends, Depends (..)) where

import Build.Task

----------------------------- Free Task Applicative ----------------------------

-- | A list of dependencies, and a function that when applied to those
-- dependencies produces the result.
data Depend k v r = Depend [k] ([v] -> r)
    deriving Functor

instance Applicative (Depend k v) where
    pure v = Depend [] (\[] -> v)
    Depend d1 f1 <*> Depend d2 f2 = Depend (d1++d2) $
        \vs -> let (v1,v2) = splitAt (length d1) vs in f1 v1 $ f2 v2

toDepend :: Task Applicative k v -> Depend k v v
toDepend f = run f $ \k -> Depend [k] $ \[v] -> v

-------------------------------- Free Task Monad -------------------------------

-- | A list of dependencies, and a function that when applied to those
-- dependencies either the result or more dependencies.
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
toDepends f = run f $ \k -> Depends [k] $ \[v] -> Done v
