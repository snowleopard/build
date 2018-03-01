{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Applicative (
    pureCompute, dependencies, transitiveDependencies, acyclic,
    Script (..), getScript, runScript
    ) where

import Control.Applicative
import Data.Maybe

import Development.Build.Compute
import Development.Build.Store
import Development.Build.Utilities

pureCompute :: (k -> v) -> Compute Applicative k v
pureCompute f _ = pure . Just . f

-- TODO: Does this always terminate? It's not obvious!
dependencies :: Compute Applicative k v -> k -> [k]
dependencies compute = getConst . compute (Const . return)

transitiveDependencies :: Eq k => Compute Applicative k v -> k -> Maybe [k]
transitiveDependencies compute = reach (dependencies compute)

acyclic :: Eq k => Compute Applicative k v -> k -> Bool
acyclic compute = isJust . transitiveDependencies compute

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

getScript :: Compute Applicative k v -> k -> Script k v (Maybe v)
getScript compute = compute GetValue

runScript :: Applicative f => (k -> f v) -> Script k v a -> f a
runScript get script = case script of
    GetValue k -> get k
    Pure v     -> pure v
    Ap s1 s2   -> runScript get s1 <*> runScript get s2
