{-# LANGUAGE FlexibleInstances, DefaultSignatures, FunctionalDependencies #-}
module Build.Store (
    -- * Hashing
    Hash, Hashable (..),

    -- * Store
    Store, values, getValue, putValue, getHash, getInfo, putInfo, mapInfo,
    initialise, agree
    ) where

import Data.List.Extra

-- TODO: Switch to cryptographic hashes.
-- | A 'Hash' is used for efficient tracking and sharing of build results. We
-- use @newtype Hash a = Hash a@ for prototyping.
newtype Hash a = Hash a deriving (Eq, Ord)

instance Functor Hash where
    fmap f (Hash a) = Hash (f a)

instance Applicative Hash where
    pure = Hash
    Hash f <*> Hash a = Hash (f a)

class Ord a => Hashable a where
    -- | Compute the hash of a given value. We typically assume cryptographic
    -- hashing, e.g. SHA256.
    hash :: a -> Hash a

instance Hashable Int where
    hash = Hash

instance Hashable Integer where
    hash = Hash

instance Hashable a => Hashable [a] where
    hash = Hash

instance Hashable a => Hashable (Hash a) where
    hash = Hash

instance (Hashable a, Hashable b) => Hashable (a, b) where
    hash = Hash

data Store i k v = Store { info :: i, values :: k -> v }

getInfo :: Store i k v -> i
getInfo = info

getValue :: k -> Store i k v -> v
getValue = flip values

getHash :: Hashable v => k -> Store i k v -> Hash v
getHash k = hash . getValue k

putInfo :: i -> Store i k v -> Store i k v
putInfo i s = s { info = i }

mapInfo :: (i -> j) -> Store i k v -> Store j k v
mapInfo f (Store i kv) = Store (f i) kv

putValue :: Eq k => k -> v -> Store i k v -> Store i k v
putValue k v s = s { values = \key -> if key == k then v else values s key }

initialise :: i -> (k -> v) -> Store i k v
initialise = Store

agree :: Eq v => [Store i k v] -> [k] -> Bool
agree ss = all same
  where
    same k = allSame [getValue k s | s <- ss]
