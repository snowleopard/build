{-# LANGUAGE ConstraintKinds, Rank2Types, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

module Neil.Execute(
    Disk(..),
    M, execute,
    ) where

import Neil.Compute
import Neil.Constraints
import qualified Neil.DynamicMap as DM
import Data.Default
import Data.Maybe
import Data.Typeable
import Control.Monad.Trans.State
import qualified Data.Map as Map

data Disk k v = Disk
    {diskTime :: Time
    ,diskStore :: Map.Map k (Time, v)
    ,diskInfo :: DM.DynamicMap
    }

instance Ord k => Monoid (Disk k v) where
    mempty = Disk mempty mempty mempty
    mappend (Disk x1 x2 x3) (Disk y1 y2 y3) = Disk (mappend x1 y1) (mappend x2 y2) (mappend x3 y3)

execute :: M k v () -> Compute Monad k v -> Disk k v -> Disk k v
execute (M m) c d = Disk (time res) (store res) (info res)
    where res = execState m $ S c (diskTime d) (diskStore d) (diskInfo d) DM.empty

data S k v = S
    {compute :: Compute Monad k v
    ,time :: Time
    ,store :: Map.Map k (Time, v)
    ,info :: DM.DynamicMap
    ,temp :: DM.DynamicMap
    }

newtype M k v r = M (State (S k v) r)
    deriving (Functor, Applicative, Monad)

instance Run (M k v) k v where
    runTrace f k = do
        S{..} <- M get
        trackDependencies compute f k

instance (Ord k, Show k) => Store (M k v) k v where
    getStoreMaybe k = fmap snd . Map.lookup k . store <$> M get
    getStore k = fromMaybe (error $ "getStore failed on " ++ show k) <$> getStoreMaybe k
    putStore k v = do
        M $ modify $ \x -> x{store = Map.insert k (time x, v) $ store x, time = succ $ time x}
        return v

instance (Ord k, Show k) => StoreTime (M k v) k where
    getStoreTimeMaybe k = fmap fst . Map.lookup k . store <$> M get
    getStoreTime k = fromMaybe (error $ "getStoreTime failed on " ++ show k) <$> getStoreTimeMaybe k

instance (Typeable t, Default t) => Temp (M k v) t where
    getTemp = fromMaybe def . DM.lookup . temp <$> M get
    putTemp t = M $ modify $ \x -> x{temp = DM.insert t $ temp x}

instance (Typeable t, Default t) => Info (M k v) t where
    getInfo = fromMaybe def . DM.lookup . info <$> M get
    putInfo t = M $ modify $ \x -> x{info = DM.insert t $ info x}


instance HasHash Int where
    getHash x = Hash x
