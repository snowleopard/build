{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving #-}

module Neil.Constraints where

import Data.Default

class Monad m => Store m k v | m -> k v where
    getStoreMaybe :: k -> m (Maybe v)
    getStore :: k -> m v
    putStore :: k -> v -> m v

newtype Time = Time Int
    deriving (Show,Eq,Ord,Enum,Default)

instance Monoid Time where
    mempty = Time 0
    mappend (Time x) (Time y) = Time $ max x y

class Monad m => StoreTime m k | k -> k where
    getStoreTimeMaybe :: k -> m (Maybe Time)
    getStoreTime :: k -> m Time

class Monad m => Run m k v | m -> k v where
    runTrace :: (k -> m v) -> k -> m ([k], Maybe v)

    run :: (k -> m v) -> k -> m (Maybe v)
    run ask k = snd <$> runTrace ask k

type Build m k v = (Store m k v, Run m k v, Ord k)

class Monad m => Temp m a where
    getTemp :: m a
    putTemp :: a -> m ()

    updateTemp :: (a -> a) -> m ()
    updateTemp f = putTemp . f =<< getTemp

class Monad m => Info m a where
    getInfo :: m a
    putInfo :: a -> m ()

    updateInfo :: (a -> a) -> m ()
    updateInfo f = putInfo . f =<< getInfo

newtype Hash v = Hash Int
    deriving Eq

class HasHash v where
    getHash :: v -> Hash v
