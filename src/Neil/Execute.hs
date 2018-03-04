{-# LANGUAGE ConstraintKinds, Rank2Types, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Neil.Execute(
    Disk(..),
    M, execute,
    ) where

import Neil.Compute
import Neil.Constraints
import qualified Neil.DynamicMap as DM
import Control.Monad.Trans.State
import qualified Data.Map as Map

data Disk k v = Disk
    {diskStore :: Map.Map k v
    ,diskInfo :: DM.DynamicMap
    }

instance Ord k => Monoid (Disk k v) where
    mempty = Disk mempty mempty
    mappend (Disk x1 x2) (Disk y1 y2) = Disk (mappend x1 y1) (mappend x2 y2)

execute :: M c k v () -> Compute c k v -> Disk k v -> Disk k v
execute (M m) c d = disk $ execState m $ S c d DM.empty

data S c k v = S
    {compute :: Compute c k v
    ,disk :: Disk k v
    ,temp :: DM.DynamicMap
    }

newtype M c k v r = M (State (S c k v) r)
    deriving (Functor, Applicative, Monad)

instance Run (M c k v) k v where

instance Store (M c k v) k v where

instance Temp (M c k v) t where

instance Info (M c k v) i where

instance HasHash Int where

instance HasTime (Timed v) where

data Timed v = Timed Time v