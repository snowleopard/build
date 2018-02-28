{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Applicative (
    Script (..), getScript, run, runWith, dependencies
    ) where

import Data.Functor.Const
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

run :: (Applicative f, Get f k v) => Script k v a -> f a
run = runWith getValue

runWith :: Applicative f => (k -> f v) -> Script k v a -> f a
runWith get script = case script of
    GetValue k -> get k
    Pure v     -> pure v
    Ap s1 s2   -> runWith get s1 <*> runWith get s2

dependencies :: Script k v a -> [k]
dependencies = getConst . runWith (Const . return)
