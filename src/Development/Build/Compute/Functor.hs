{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Functor (
    FunctorialCompute, dependency, inputs, acyclic,
    Script (..), getScript, runScript
    ) where

import Data.Functor.Const

import Development.Build.Compute
import Development.Build.Store

-- TODO: One possible example is configuration files where we keep building
-- projections from a large collection of settings to smaller and smaller items
-- in a lens-like manner.

dependency :: FunctorialCompute k v -> k -> k
dependency compute = getConst . compute Const

-- FunctorialCompute is always cyclic!
inputs :: FunctorialCompute k v -> k -> Maybe [k]
inputs _ _ = Nothing

acyclic :: FunctorialCompute k v -> k -> Bool
acyclic _ _ = True

data Script k v a where
    GetValue :: k -> Script k v v
    FMap     :: (a -> b) -> Script k v a -> Script k v b

instance Get (Script k v) k v where
    getValue = GetValue

instance Functor (Script k v) where
    fmap = FMap

getScript :: FunctorialCompute k v -> k -> Script k v (Maybe v)
getScript compute = compute GetValue

runScript :: Monad f => (k -> f v) -> Script k v a -> f a
runScript get script = case script of
    GetValue k -> get k
    FMap f s   -> f <$> runScript get s
