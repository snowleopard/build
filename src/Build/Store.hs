-- | A key/value Store.
module Build.Store (
    -- * Hashing
    Hash, Hashable (..),

    -- * Store
    Store, getValue, putValue, getHash, getInfo, putInfo, mapInfo,
    initialise
    ) where

-- | A 'Hash' is used for efficient tracking and sharing of build results. We
-- use @newtype Hash a = Hash a@ for prototyping.
newtype Hash a = Hash a deriving (Eq, Ord,Show)

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

-- | An abstract datatype for a key/value store with build information of type @i@.
data Store i k v = Store { info :: i, values :: k -> v }

-- | Read the build information.
getInfo :: Store i k v -> i
getInfo = info

-- | Read the value of a key.
getValue :: k -> Store i k v -> v
getValue = flip values

-- | Read the hash of a key's value. In some cases may be implemented more
-- efficiently than @hash . getValue k@.
getHash :: Hashable v => k -> Store i k v -> Hash v
getHash k = hash . getValue k

-- | Write the build information.
putInfo :: i -> Store i k v -> Store i k v
putInfo i s = s { info = i }

-- | Modify the build information.
mapInfo :: (i -> j) -> Store i k v -> Store j k v
mapInfo f (Store i kv) = Store (f i) kv

-- | Update the value of a key.
putValue :: Eq k => k -> v -> Store i k v -> Store i k v
putValue k v s = s { values = \key -> if key == k then v else values s key }

-- | Initialise the store.
initialise :: i -> (k -> v) -> Store i k v
initialise = Store
