{-# LANGUAGE ConstraintKinds, DeriveFunctor, FlexibleInstances #-}
{-# LANGUAGE RankNTypes, StandaloneDeriving #-}
module Build.Task.Wrapped (GTask (..), Wrapped, unwrap) where

import Control.Applicative
import Control.Monad

import Build.Task

-- This whole module is just a tiresome workaround for the lack of impredicative
-- polymorphism. If GHC adds impredicative polymorphism, we can drop it entirely
-- and simplify the rest of the code by removing unnecessary task unwrapping.

-- GTask is a generalised Task wrapped in a newtype. It is generalised in the
-- sense that it computes a value of type @a@ given a fetch of type @k -> f v@.
newtype GTask c k v a =
    GTask { runGTask :: forall f. c f => (k -> f v) -> f a }

type Wrapped c k v = (k -> GTask c k v v) -> GTask c k v v

unwrap :: forall c k v. Wrapped c k v -> Task c k v
unwrap wrapped = runGTask (wrapped f)
  where
    f :: k -> GTask c k v v
    f k = GTask $ \f -> f k

-- Thanks to the generalisation, we can make GTask an instance of many classes
deriving instance Functor (GTask Functor     k v)
deriving instance Functor (GTask Applicative k v)
deriving instance Functor (GTask Alternative k v)
deriving instance Functor (GTask Monad       k v)
deriving instance Functor (GTask MonadPlus   k v)

instance Applicative (GTask Applicative k v) where
    pure x = GTask $ \_ -> pure x
    GTask f <*> GTask x = GTask $ \fetch -> f fetch <*> x fetch

instance Applicative (GTask Alternative k v) where
    pure x = GTask $ \_ -> pure x
    GTask f <*> GTask x = GTask $ \fetch -> f fetch <*> x fetch

instance Applicative (GTask Monad k v) where
    pure x = GTask $ \_ -> pure x
    GTask f <*> GTask x = GTask $ \fetch -> f fetch <*> x fetch

instance Applicative (GTask MonadPlus k v) where
    pure x = GTask $ \_ -> pure x
    GTask f <*> GTask x = GTask $ \fetch -> f fetch <*> x fetch

instance Monad (GTask Monad k v) where
    return x = GTask $ \_ -> return x
    GTask x >>= f = GTask $ \fetch -> x fetch >>= \a -> runGTask (f a) fetch

instance Monad (GTask MonadPlus k v) where
    return x = GTask $ \_ -> return x
    GTask x >>= f = GTask $ \fetch -> x fetch >>= \a -> runGTask (f a) fetch

instance Alternative (GTask Alternative k v) where
    empty = GTask $ \_ -> empty
    GTask x <|> GTask y = GTask $ \fetch -> x fetch <|> y fetch

instance Alternative (GTask MonadPlus k v) where
    empty = GTask $ \_ -> empty
    GTask x <|> GTask y = GTask $ \fetch -> x fetch <|> y fetch

instance MonadPlus (GTask MonadPlus k v) where
    mzero = empty
    mplus = (<|>)
