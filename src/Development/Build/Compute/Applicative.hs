{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Applicative (
    ApplicativeCompute, pureCompute, dependencies, Script (..), getScript, runScript
    ) where

import Control.Applicative

import Development.Build.Compute
import Development.Build.Store

pureCompute :: (k -> v) -> ApplicativeCompute k v
pureCompute f _ = pure . f

dependencies :: ApplicativeCompute k v -> k -> [k]
dependencies compute = getConst . compute (Const . return)

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

getScript :: ApplicativeCompute k v -> k -> Script k v v
getScript compute = compute GetValue

runScript :: Applicative f => (k -> f v) -> Script k v a -> f a
runScript get script = case script of
    GetValue k -> get k
    Pure v     -> pure v
    Ap s1 s2   -> runScript get s1 <*> runScript get s2
