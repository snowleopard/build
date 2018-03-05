{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, ConstraintKinds #-}

module Neil.Build(
    Build,
    M, runM,
    getStoreMap, getStoreMaybe, getStore, putStore,
    getInfo, putInfo, updateInfo,
    getTemp, putTemp, updateTemp,
    Hash, getHash, Hashable,
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


type Build c k v i = (Ord k, Show k, Typeable k) => Compute c k v -> [k] -> Maybe i -> Map.Map k v -> (i, Map.Map k v)


runM :: Default i => M k v i () -> Maybe i -> Map.Map k v -> (i, Map.Map k v)
runM (M m) i s = (info res, store res)
    where res = execState m $ S s (fromMaybe def i) mempty

data S k v i = S
    {store :: Map.Map k v
    ,info :: i
    ,temp :: DM.DynamicMap
    }

newtype M k v i r = M (State (S k v i) r)
    deriving (Functor, Applicative, Monad)


getStoreMap :: M k v i (Map.Map k v)
getStoreMap = M $ gets store

getStoreMaybe :: Ord k => k -> M k v i (Maybe v)
getStoreMaybe k = Map.lookup k <$> getStoreMap

getStore :: (Ord k, Show k) => k -> M k v i v
getStore k = fromMaybe (error $ "getStore failed on " ++ show k) <$> getStoreMaybe k

putStore :: Ord k => k -> v -> M k v i v
putStore k v = do
    M $ modify $ \x -> x{store = Map.insert k v $ store x}
    return v

getTemp :: (Typeable t, Default t) => M k v i t
getTemp = fromMaybe def . DM.lookup . temp <$> M get

putTemp :: Typeable t => t -> M k v i ()
putTemp t = M $ modify $ \x -> x{temp = DM.insert t $ temp x}

updateTemp :: (Typeable t, Default t) => (t -> t) -> M k v s ()
updateTemp f = putTemp . f =<< getTemp

getInfo :: M k v i i
getInfo = info <$> M get

putInfo :: i -> M k v i ()
putInfo i = M $ modify $ \x -> x{info = i}

updateInfo :: (i -> i) -> M k v i ()
updateInfo f = putInfo . f =<< getInfo


newtype Hash v = Hash Int
    deriving Eq

getHash :: Hashable v => v -> Hash v
getHash = Hash . hash
