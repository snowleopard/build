{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Applicative (
    Script (..), getScript, runScript, dependencies
    ) where

import Development.Build.Store

data Script k v a where
    GetValue :: k -> Script k v v
    Pure     :: a -> Script k v a
    Ap       :: Script k v (a -> b) -> Script k v a -> Script k v b

instance Get (Script k v) k v where
    getValue = GetValue

instance Functor (Script k v) where
    fmap = Ap . Pure

instance Applicative (Script k v) where
    pure  = Pure
    (<*>) = Ap

getScript :: (forall f. (Applicative f, Get f k v) => k -> f v) -> k -> Script k v v
getScript = id

runScript :: (Applicative f, Get f k v) => Script k v a -> f a
runScript script = case script of
    GetValue k -> getValue k
    Pure v     -> pure v
    Ap s1 s2   -> runScript s1 <*> runScript s2

dependencies :: Script k v a -> [k]
dependencies script = case script of
    GetValue k -> [k]
    Pure _     -> []
    Ap s1 s2   -> dependencies s1 ++ dependencies s2
