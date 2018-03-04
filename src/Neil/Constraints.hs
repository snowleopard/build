{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, FunctionalDependencies #-}

module Neil.Constraints where

class Monad m => Store m k v | m -> k v where
    getStoreMaybe :: k -> m (Maybe v)
    getStore :: k -> m v
    putStore :: k -> v -> m v

class Monad m => Run m k v | m -> k v where
    runTrace :: (k -> m v) -> k -> m ([k], Maybe v)

    run :: (k -> m v) -> k -> m (Maybe v)
    run ask k = snd <$> runTrace ask k

type Build m k v = (Store m k v, Run m k v, Ord k)

class PreDepends m k | m -> k where
    depends :: m (k -> [k])

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


newtype Time = Time Int
    deriving (Eq,Ord)

class HasTime v where
    getTime :: v -> Time

newtype Hash v = Hash Int
    deriving Eq

class HasHash v where
    getHash :: v -> Hash v
