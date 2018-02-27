{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Functor (
    Script (..), normalForm, getScript, runScript
    ) where

import Development.Build.Store

-- TODO: One possible example is configuration files where we keep building
-- projections from a large collection of settings to smaller and smaller items
-- in a lens-like manner.

data Script k v a where
    GetValue :: k -> Script k v v
    FMap     :: (a -> b) -> Script k v a -> Script k v b

normalForm :: Script k v a -> (k, v -> a)
normalForm (GetValue key ) = (key, id)
normalForm (FMap f script) = (key, f . g)
  where
    (key, g) = normalForm script

instance Get (Script k v) k v where
    getValue = GetValue

instance Functor (Script k v) where
    fmap = FMap

getScript :: (forall f. (Functor f, Get f k v) => k -> f v) -> k -> Script k v v
getScript = id

runScript :: (Functor f, Get f k v) => (k -> Script k v v) -> k -> f v
runScript script key = fmap f (getValue k)
  where
    (k, f) = normalForm (script key)
