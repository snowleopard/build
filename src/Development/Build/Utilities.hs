{-# LANGUAGE TupleSections #-}
module Development.Build.Utilities (
    -- Tracing functors
    trace, wrap, writeExternal, writeInternal, runTrace,

    -- * Logic combinators
    forall, forallM, exists, existsM, (==>)
    ) where

import Control.Monad.Trans.Writer
import Data.Monoid

wrap :: (Functor f, Monoid w) => f a -> WriterT w f a
wrap f = WriterT $ fmap (,mempty) f

writeExternal :: (Functor f, Monoid w) => w -> WriterT w f a -> WriterT w f a
writeExternal x (WriterT f) = WriterT $ fmap (\(a, w) -> (a, w <> x)) f

writeInternal :: (Functor f, Monoid w) => (a -> w) -> WriterT w f a -> WriterT w f a
writeInternal g (WriterT f) = WriterT $ fmap (\(a, w) -> (a, w <> g a)) f

trace :: Functor f => (k -> f v) -> k -> WriterT [(k, v)] f v
trace lookup key = writeInternal (\v -> [(key, v)]) $ wrap $ lookup key

runTrace :: WriterT [(k, v)] f a -> f (a, [(k, v)])
runTrace = runWriterT

-- | Check that a predicate holds for all values of @a@.
forall :: (a -> Bool) -> Bool
forall = undefined

-- | Check that a monadic predicate holds for all values of @a@.
forallM :: (a -> m Bool) -> m Bool
forallM = undefined

-- | Check that a predicate holds for some value of @a@.
exists :: (a -> Bool) -> Bool
exists = undefined

-- | Check that a monadic predicate holds for some value of @a@.
existsM :: (a -> m Bool) -> m Bool
existsM = undefined

-- | Logical implication.
(==>) :: Bool -> Bool -> Bool
x ==> y = if x then y else True

infixr 0 ==>
