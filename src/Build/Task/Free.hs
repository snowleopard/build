{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | The free description of tasks.
module Build.Task.Free (
    Rule (..), toRule, fromRule, Action (..), toAction, fromAction
  ) where

import Build.Task
import Control.Monad

------------------------- Isomorphism with Make's Rule -------------------------
data Rule k v r = Rule [k] ([v] -> r)
    deriving Functor

instance Applicative (Rule k v) where
    pure v = Rule [] (\[] -> v)
    Rule d1 f1 <*> Rule d2 f2 = Rule (d1++d2) $ \vs ->
        let (v1,v2) = splitAt (length d1) vs in f1 v1 $ f2 v2

getRule :: k -> Rule k v v
getRule k = Rule [k] $ \[v] -> v

toRule :: Task Applicative k v -> Rule k v v
toRule (Task f) = f getRule

fromRule :: Rule k v v -> Task Applicative k v
fromRule (Rule ds f) = Task $ \fetch -> f <$> traverse fetch ds

------------------------ Isomorphism with Shake's Action -----------------------
data Action k v a = Finished a
                  | Depends k (v -> Action k v a)
    deriving Functor

instance Applicative (Action k v) where
    pure  = Finished
    (<*>) = ap

instance Monad (Action k v) where
    return = Finished
    Finished x    >>= f = f x
    Depends ds op >>= f = Depends ds (op >=> f)

toAction :: Task Monad k v -> Action k v v
toAction (Task run) = run $ \k -> Depends k Finished

fromAction :: Action k v v -> Task Monad k v
fromAction x = Task $ \fetch -> f fetch x
  where
    f _     (Finished v  ) = return v
    f fetch (Depends d op) = fetch d >>= f fetch . op
