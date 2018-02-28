{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Functor (
    Script (..), getScript, run, runWith, dependency
    ) where

import Data.Functor.Const
import Development.Build.Store

-- TODO: One possible example is configuration files where we keep building
-- projections from a large collection of settings to smaller and smaller items
-- in a lens-like manner.

data Script k v a where
    GetValue :: k -> Script k v v
    FMap     :: (a -> b) -> Script k v a -> Script k v b

instance Get (Script k v) k v where
    getValue = GetValue

instance Functor (Script k v) where
    fmap = FMap

getScript :: (forall f. (Functor f, Get f k v) => k -> f v) -> k -> Script k v v
getScript = id

run :: (Functor f, Get f k v) => Script k v a -> f a
run = runWith getValue

runWith :: Functor f => (k -> f v) -> Script k v a -> f a
runWith get script = case script of
    GetValue k -> get k
    FMap f s   -> f <$> runWith get s

dependency :: Script k v a -> k
dependency = getConst . runWith Const
