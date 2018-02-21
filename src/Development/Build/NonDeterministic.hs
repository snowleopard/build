{-# LANGUAGE DeriveFunctor #-}
module Development.Build.NonDeterministic (NonDeterministic, member, pick) where

import Control.Monad
import Data.List.NonEmpty

-- | Type for a non-deterministic computation whose result comes from a non-empty
-- set of valid results.
newtype NonDeterministic a = NonDeterministic { validResults :: NonEmpty a }
    deriving Functor

instance Applicative NonDeterministic where
    pure x = NonDeterministic (x :| [])
    (<*>)  = ap

instance Monad NonDeterministic where
    return = pure
    NonDeterministic xs >>= f = NonDeterministic (xs >>= validResults . f)

-- | Check if a value is a valid result of a non-deterministic computation.
member :: Eq a => a -> NonDeterministic a -> Bool
member x = elem x . validResults

-- | Choose a valid result.
pick :: NonDeterministic a -> a
pick (NonDeterministic (first :| _)) = first
