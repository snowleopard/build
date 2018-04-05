{-# LANGUAGE FlexibleInstances, DefaultSignatures, FunctionalDependencies #-}
module Build.Store (
    -- * Hashing
    Hash (..), Hashable (..),

    -- * Store
    Store, getValue, putValue, getHash, getInfo, putInfo, mapInfo,
    initialise, checkHashes, agree

    -- * The store monad
    -- Store (..), Snapshot (..), checkHashes, PureStore, runPureStore,
    -- UnsafeMapStore, runUnsafeMapStore, MapStore, runMapStoreT, runMapStore
    ) where

import Data.Semigroup

-- | A 'Hash' is used for efficient tracking and sharing of build results. We
-- use @newtype Hash a = Hash Int@ for prototyping.
newtype Hash a = Hash { unhash :: Int } deriving (Eq,Ord)

instance Semigroup (Hash a) where
    Hash x <> Hash y = Hash (x * (y + 1))

instance Hashable a => Monoid (Hash a) where
    mempty  = Hash 0
    mappend = (<>)

class Hashable a where
    -- | Compute the hash of a given value. We typically assume cryptographic
    -- hashing, e.g. SHA256.
    hash :: a -> Hash a

instance Hashable Int where
    hash = Hash

instance Hashable Integer where
    hash = Hash . fromIntegral

instance Hashable a => Hashable [a] where
    hash = Hash . unhash . mconcat . map hash

instance (Hashable a, Hashable b) => Hashable (a, b) where
    hash (a, b) = Hash (unhash (hash a) * (unhash (hash b) + 2))

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

checkHashes :: Hashable v => Store m k v -> [(k, Hash v)] -> Bool
checkHashes store = all (\(k, h) -> getHash k store == h)

agree :: Eq v => [Store i k v] -> [k] -> Bool
agree ss = all same
  where
    same k = let vs = [getValue k s | s <- ss] in and $ zipWith (==) vs (drop 1 vs)
