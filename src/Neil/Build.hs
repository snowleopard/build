{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, ConstraintKinds #-}

module Neil.Build(
    Build,
    M, runM,
    getStore, putStore,
    Changed, getChanged, withChangedApplicative, withChangedMonad,
    Time, getStoreTime,
    getInfo, modifyInfo,
    getTemp, modifyTemp,
    Hash, getHash, Hashable, getStoreHash,
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


type Build c i k v = (Ord k, Typeable k) => Compute c k v -> k -> Maybe i -> (k -> v) -> (i, k -> v)


runM :: (Default i) => M i k v a -> Maybe i -> (k -> v) -> (i, k -> v)
runM (M m) i s = (info res, store res)
    where res = execState m $ S s (fromMaybe def i) mempty Set.empty

data S i k v = S
    {store :: k -> v
    ,info :: i
    ,temp :: DM.DynamicMap
    ,changed :: Set.Set k
    }

newtype M i k v r = M (State (S i k v) r)
    deriving (Functor, Applicative, Monad)


-- | Figure out when files change, like a modtime
newtype Changed k v = Changed {hasChanged :: k -> v -> Bool}

instance Show (Changed k v) where
    show _ = "<Changed>"

instance Default (Changed k v) where
    def = Changed $ \_ _ -> True

getChanged :: (Ord k, Eq v) => k -> M (Changed k v, i) k v Bool
getChanged k = do
    s <- M get
    return $ k `Set.member` changed s || hasChanged (fst $ info s) k (store s k)

withChangedApplicative :: Eq v => Build Applicative (Changed k v, i) k v -> Build Applicative (Changed k v, i) k v
withChangedApplicative op compute k i mp = let ((_, i2),mp2) = op compute k i mp in ((Changed $ \k v -> mp2 k /= v, i2), mp2)

withChangedMonad :: Eq v => Build Monad (Changed k v, i) k v -> Build Monad (Changed k v, i) k v
withChangedMonad op compute k i mp = let ((_, i2),mp2) = op compute k i mp in ((Changed $ \k v -> mp2 k /= v, i2), mp2)

data Time = LastBuild | AfterLastBuild deriving (Eq,Ord)

getStoreTime :: (Ord k, Eq v) => k -> M (Changed k v, i) k v Time
getStoreTime k = do
    chng <- getChanged k
    return $ if chng then AfterLastBuild else LastBuild

getStore :: k -> M i k v v
getStore k = ($ k) . store <$> M get

putStore :: Ord k => k -> v -> M i k v ()
putStore k v = M $ modify $ \x -> x{store = \k2 -> if k == k2 then v else store x k2, changed = Set.insert k $ changed x}

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

getStoreHash :: (Hashable v) => k -> M i k v (Hash v)
getStoreHash = fmap getHash . getStore

