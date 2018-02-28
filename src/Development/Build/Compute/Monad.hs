{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Monad (
    Script (..), getScript, runScript, staticDependencies
    ) where

import Development.Build.Store

data Script k v a where
    GetValue :: k -> Script k v v
    Pure     :: a -> Script k v a
    Ap       :: Script k v (a -> b) -> Script k v a -> Script k v b
    Bind     :: Script k v a -> (a -> Script k v b) -> Script k v b

instance Get (Script k v) k v where
    getValue = GetValue

instance Functor (Script k v) where
    fmap = Ap . Pure

instance Applicative (Script k v) where
    pure  = Pure
    (<*>) = Ap

instance Monad (Script k v) where
    return = Pure
    (>>=)  = Bind

getScript :: (forall m. (Monad m, Get m k v) => k -> m v) -> k -> Script k v v
getScript = id

runScript :: (Monad m, Get m k v) => Script k v a -> m a
runScript script = case script of
    GetValue k -> getValue k
    Pure v     -> pure v
    Ap s1 s2   -> runScript s1 <*> runScript s2
    Bind s f   -> runScript (runScript s >>= f)

staticDependencies :: Script k v a -> [k]
staticDependencies script = case script of
    GetValue k -> [k]
    Pure _     -> []
    Ap s1 s2   -> staticDependencies s1 ++ staticDependencies s2
    Bind s _   -> staticDependencies s
