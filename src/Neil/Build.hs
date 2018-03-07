{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, ConstraintKinds #-}

module Neil.Build(
    Build,
    M, runM,
    getStoreMap,
    getStoreMaybe, getStore, putStore, putStore_,
    getInfo, putInfo, modifyInfo,
    getTemp, putTemp, modifyTemp,
    Hash, getHash, Hashable, getStoreHash, getStoreHashMaybe,
    ) where

import Neil.Compute
import Control.Monad.Extra
import qualified Neil.DynamicMap as DM
import Data.Typeable
import Data.Hashable
import Control.Monad.Trans.State
import Data.Maybe
import Data.Default
import qualified Data.Set as Set
import qualified Data.Map as Map


type Build c i k v = (Ord k, Show k, Typeable k) => Compute c k v -> k -> Maybe i -> Map.Map k v -> (i, Map.Map k v)


runM :: Default i => M i k v a -> Maybe i -> Map.Map k v -> (i, Map.Map k v)
runM (M m) i s = (info res, store res)
    where res = execState m $ S s (fromMaybe def i) mempty

data S i k v = S
    {store :: Map.Map k v
    ,info :: i
    ,temp :: DM.DynamicMap
    }

newtype M i k v r = M (State (S i k v) r)
    deriving (Functor, Applicative, Monad)


getStoreMap :: M i k v (Map.Map k v)
getStoreMap = M $ gets store

getStoreMaybe :: Ord k => k -> M i k v (Maybe v)
getStoreMaybe k = Map.lookup k <$> getStoreMap

getStore :: (Ord k, Show k) => k -> M i k v v
getStore k = fromMaybe (error $ "getStore failed on " ++ show k) <$> getStoreMaybe k

putStore :: Ord k => k -> v -> M i k v v
putStore k v = do putStore_ k v; return v

putStore_ :: Ord k => k -> v -> M i k v ()
putStore_ k v = M $ modify $ \x -> x{store = Map.insert k v $ store x}

getTemp :: (Typeable t, Default t) => M i k v t
getTemp = fromMaybe def . DM.lookup . temp <$> M get

putTemp :: Typeable t => t -> M i k v ()
putTemp t = M $ modify $ \x -> x{temp = DM.insert t $ temp x}

modifyTemp :: (Typeable t, Default t) => (t -> t) -> M k v s ()
modifyTemp f = putTemp . f =<< getTemp

getInfo :: M i k v i
getInfo = info <$> M get

putInfo :: i -> M i k v ()
putInfo i = M $ modify $ \x -> x{info = i}

modifyInfo :: (i -> i) -> M i k v ()
modifyInfo f = putInfo . f =<< getInfo


newtype Hash v = Hash Int
    deriving (Eq,Ord,Show)

getHash :: Hashable v => v -> Hash v
getHash = Hash . hash

getStoreHashMaybe :: (Ord k, Hashable v) => k -> M i k v (Maybe (Hash v))
getStoreHashMaybe = fmap (fmap getHash) . getStoreMaybe

getStoreHash :: (Ord k, Show k, Hashable v) => k -> M i k v (Hash v)
getStoreHash = fmap getHash . getStore

