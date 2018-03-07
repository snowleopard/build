{-# LANGUAGE Rank2Types, ConstraintKinds, DeriveFunctor, KindSignatures #-}

module Neil.Compute(
    Compute,
    isInput,
    getDependencies, getDependenciesMaybe, trackDependencies, failDependencies,
    Depend(..), runDepend, toDepend,
    Depends(..), runDepends, toDepends,
    ) where

import Data.Kind
import Data.Functor.Const
import Data.Monoid
import Data.Maybe
import Data.Proxy
import Data.Tuple.Extra


type Compute c k v = forall f . c f => (k -> f v) -> k -> Maybe (f v)

isInput :: Compute Monad k v -> k -> Bool
isInput comp = isNothing . comp (const Proxy)

getDependencies :: Compute Applicative k v -> k -> [k]
getDependencies comp = maybe [] getConst . comp (\k -> Const [k])

getDependenciesMaybe :: Compute Monad k v -> k -> Maybe [k]
getDependenciesMaybe comp = maybe (Just []) getConstMaybe . comp (\x -> ConstMaybe $ Just [x])

trackDependencies :: Monad m => Compute Monad k v -> (k -> m v) -> k -> Maybe (m ([k], v))
trackDependencies comp f k = runDepends f <$> toDepends comp k

failDependencies :: Monad m => Compute Monad k v -> (k -> m (Either e v)) -> k -> Maybe (m (Either e v))
failDependencies comp f k = go <$> toDepends comp k
    where go (Done r) = return $ Right r
          go (Depends ds next) = do
                vs <- mapM f ds
                case sequence vs of
                    Left e -> return $ Left e
                    Right xs -> go $ next xs


data ConstMaybe a b = ConstMaybe {getConstMaybe :: Maybe a}
    deriving Functor

instance Monoid m => Applicative (ConstMaybe m) where
    pure _ = ConstMaybe $ Just mempty
    ConstMaybe f <*> ConstMaybe x = ConstMaybe $ case (f,x) of
        (Just f, Just x) -> Just $ f <> x
        _ -> Nothing

instance Monoid m => Monad (ConstMaybe m) where
    return = pure
    a >> b = a *> b
    a >>= b = ConstMaybe Nothing


data Depend k v r = Depend [k] ([v] -> r)
    deriving Functor

runDepend :: Applicative m => (k -> m v) -> Depend k v r -> ([k], m r)
runDepend f (Depend ds op) = (ds, op <$> traverse f ds)

instance Applicative (Depend k v) where
    pure v = Depend [] (\[] -> v)
    Depend d1 f1 <*> Depend d2 f2 = Depend (d1++d2) $ \vs -> let (v1,v2) = splitAt (length d1) vs in f1 v1 $ f2 v2


data Depends k v r = Depends [k] ([v] -> Depends k v r)
                   | Done r
    deriving Functor

runDepends :: Monad m => (k -> m v) -> Depends k v r -> m ([k], r)
runDepends f (Depends ds op) = first (ds++) <$> (runDepends f . op =<< mapM f ds)
runDepends f (Done r) = pure ([], r)

instance Applicative (Depends k v) where
    pure = return
    f1 <*> f2 = f2 >>= \v -> ($ v) <$> f1

instance Monad (Depends k v) where
    return = Done
    Done x >>= f = f x
    Depends ds op >>= f = Depends ds $ \vs -> f =<< op vs


{-
-- ALTERNATIVE DEFINITION OF DEPENDS
-- You can define Depends in terms of Depend, but it doesn't make it very clear!

newtype Depends k v r = Depends (Compose (Depend k v) (Step (Depends k v)) r)
    deriving newtype (Functor, Applicative)

data Step f r = More (f r) | Done r
    deriving (Functor)

fromMore :: Applicative f => Step f r -> f r
fromMore (Done x) = pure x
fromMore (More x) = x

instance Applicative f => Applicative (Step f) where
    pure x = Done x
    Done f1 <*> Done f2 = Done $ f1 f2
    (fromMore -> f1) <*> (fromMore -> f2) = More $ f1 <*> f2
-}


getDepend :: k -> Depend k v v
getDepend k = Depend [k] $ \[v] -> v

toDepend :: Compute Applicative k v -> k -> Maybe (Depend k v v)
toDepend f = f getDepend

getDepends :: k -> Depends k v v
getDepends k = Depends [k] $ \[v] -> Done v

toDepends :: Compute Monad k v -> k -> Maybe (Depends k v v)
toDepends f = f getDepends
