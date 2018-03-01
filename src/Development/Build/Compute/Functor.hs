{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Functor (
    dependency, transitiveDependencies, acyclic, Script (..), getScript, runScript
    ) where

import Data.Functor.Const

import Development.Build.Compute
import Development.Build.Store

dependency :: Compute Functor k v -> k -> k
dependency compute = getConst . compute Const

-- Compute Functor is always cyclic! They can't declare any keys as input
-- as they have no way to lift Nothing into the functor. They can do
--
-- compute get k = fmap (const Nothing) (get k)
--
-- But this still registers as a dependency on k even though the result is discarded.
transitiveDependencies :: Compute Functor k v -> k -> Maybe [k]
transitiveDependencies _ _ = Nothing

acyclic :: Compute Functor k v -> k -> Bool
acyclic _ _ = False

data Script k v a where
    GetValue :: k -> Script k v v
    FMap     :: (a -> b) -> Script k v a -> Script k v b

instance Get (Script k v) k v where
    getValue = GetValue

instance Functor (Script k v) where
    fmap = FMap

getScript :: Compute Functor k v -> k -> Script k v (Maybe v)
getScript compute = compute GetValue

runScript :: Monad f => (k -> f v) -> Script k v a -> f a
runScript get script = case script of
    GetValue k -> get k
    FMap f s   -> f <$> runScript get s
