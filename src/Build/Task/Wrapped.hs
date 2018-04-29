{-# LANGUAGE ConstraintKinds, DeriveFunctor, FlexibleInstances #-}
{-# LANGUAGE RankNTypes, StandaloneDeriving #-}
module Build.Task.Wrapped (ReifiedTask (..), Wrapped) where

import Control.Applicative
import Control.Monad

-- This whole module is just a tiresome workaround for the lack of impredicative
-- polymorphism.

newtype ReifiedTask c k v a =
    ReifiedTask { runTask :: forall f. c f => (k -> f v) -> f a }

type Wrapped c k v = (k -> ReifiedTask c k v v) -> ReifiedTask c k v v

deriving instance Functor (ReifiedTask Functor     k v)
deriving instance Functor (ReifiedTask Applicative k v)
deriving instance Functor (ReifiedTask Alternative k v)
deriving instance Functor (ReifiedTask Monad       k v)
deriving instance Functor (ReifiedTask MonadPlus   k v)

instance Applicative (ReifiedTask Applicative k v) where
    pure x = ReifiedTask $ \_ -> pure x
    ReifiedTask f <*> ReifiedTask x = ReifiedTask $ \fetch -> f fetch <*> x fetch

instance Applicative (ReifiedTask Alternative k v) where
    pure x = ReifiedTask $ \_ -> pure x
    ReifiedTask f <*> ReifiedTask x = ReifiedTask $ \fetch -> f fetch <*> x fetch

instance Applicative (ReifiedTask Monad k v) where
    pure x = ReifiedTask $ \_ -> pure x
    ReifiedTask f <*> ReifiedTask x = ReifiedTask $ \fetch -> f fetch <*> x fetch

instance Applicative (ReifiedTask MonadPlus k v) where
    pure x = ReifiedTask $ \_ -> pure x
    ReifiedTask f <*> ReifiedTask x = ReifiedTask $ \fetch -> f fetch <*> x fetch

instance Monad (ReifiedTask Monad k v) where
    return x = ReifiedTask $ \_ -> return x
    ReifiedTask x >>= f = ReifiedTask $ \fetch -> x fetch >>= \a -> runTask (f a) fetch

instance Monad (ReifiedTask MonadPlus k v) where
    return x = ReifiedTask $ \_ -> return x
    ReifiedTask x >>= f = ReifiedTask $ \fetch -> x fetch >>= \a -> runTask (f a) fetch

instance Alternative (ReifiedTask Alternative k v) where
    empty = ReifiedTask $ \_ -> empty
    ReifiedTask x <|> ReifiedTask y = ReifiedTask $ \fetch -> x fetch <|> y fetch

instance Alternative (ReifiedTask MonadPlus k v) where
    empty = ReifiedTask $ \_ -> empty
    ReifiedTask x <|> ReifiedTask y = ReifiedTask $ \fetch -> x fetch <|> y fetch

instance MonadPlus (ReifiedTask MonadPlus k v) where
    mzero = empty
    mplus = (<|>)
