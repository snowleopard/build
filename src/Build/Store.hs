{-# LANGUAGE FlexibleInstances, DefaultSignatures, FunctionalDependencies #-}
module Build.Store (
    -- * Hashing
    Hash, Hashable (..),

    -- * Store
    Store, getValue, putValue, getHash, getInfo, putInfo, initialise, mapInfo,
    checkHashes, agree

    -- * The store monad
    -- Store (..), Snapshot (..), checkHashes, PureStore, runPureStore,
    -- UnsafeMapStore, runUnsafeMapStore, MapStore, runMapStoreT, runMapStore
    ) where

-- | A 'Hash' is used for efficient tracking and sharing of build results. We
-- use @newtype Hash a = Hash Int@ for prototyping.
newtype Hash a = Hash Int deriving (Eq,Ord)

class Hashable a where
    -- | Compute the hash of a given value. We typically assume cryptographic
    -- hashing, e.g. SHA256.
    hash :: a -> Hash a

instance Hashable Int where
    hash = Hash

instance Hashable Integer where
    hash = Hash . fromIntegral

data Store i k v = Store { info :: i, values :: k -> v }

getInfo :: Store i k v -> i
getInfo = info

getValue :: k -> Store i k v -> v
getValue = flip values

getHash :: Hashable v => k -> Store i k v -> Hash v
getHash k = hash . getValue k

putInfo :: i -> Store i k v -> Store i k v
putInfo i s = s { info = i }

putValue :: Eq k => k -> v -> Store i k v -> Store i k v
putValue k v s = s { values = \key -> if key == k then v else values s key }

initialise :: i -> (k -> v) -> Store i k v
initialise = Store

mapInfo :: (i -> j) -> Store i k v -> Store j k v
mapInfo f (Store i kv) = Store (f i) kv

checkHashes :: Hashable v => Store m k v -> [(k, Hash v)] -> Bool
checkHashes store = all (\(k, h) -> getHash k store == h)

agree :: Eq v => [Store i k v] -> [k] -> Bool
agree ss = all same
  where
    same k = let vs = [getValue k s | s <- ss] in and $ zipWith (==) vs (drop 1 vs)


-- | A key-value store monad that in addition to usual 'getValue' and 'putValue'
-- queries supports 'getHash', which can in some cases be implemented more
-- efficiently than by hashing the result of 'getValue'. For example, GVFS (Git
-- Virtual File System) downloads actual values (file contents) only on demand.
-- Note that if a file does not exist, the corresponding 'file not found' value
-- (suitably encoded) is still useful and can be tracked by a build system.
-- class Monad m => Store m k v | m -> k v where
--     getValue :: k -> m v
--     getHash :: k -> m Hash
--     getHash = fmap hash . getValue
--     putValue :: k -> v -> m ()

-- class Store m k v => Snapshot m k v where
--     loadSnapshot :: (k -> v) -> m ()
--     saveSnapshot :: m (k -> v)

-- -- | A 'Store' implemented using a function @k -> v@.
-- type PureStore m k v = StateT (k -> v) m

-- instance (Eq k, Monad m) => Store (PureStore m k v) k v where
--     getValue k   = ($k) <$> get
--     putValue k v = modify (\lookup key -> if key == k then v else lookup key)

-- -- | Run an 'UnsafeMapStore' computation on a given initial state of the store,
-- -- Throws an error when accessing a non-existent key. See 'runMapStore' for a
-- -- safe alternative.
-- runPureStore :: PureStore m k v a -> (k -> v) -> m (a, k -> v)
-- runPureStore = runStateT

-- -- | A 'Store' implemented using a @Map k v@. Throws an error when accessing a
-- -- non-existent key. See 'MapStore' for a safe alternative.
-- type UnsafeMapStore m k v = StateT (Map k v) m

-- instance (Ord k, Monad m) => Store (UnsafeMapStore m k v) k v where
--     getValue k   = (!k) <$> get
--     putValue k v = modify (insert k v)

-- -- | Run an 'UnsafeMapStore' computation on a given initial state of the store,
-- -- Throws an error when accessing a non-existent key. See 'runMapStore' for a
-- -- safe alternative.
-- runUnsafeMapStore :: UnsafeMapStore m k v a -> Map k v -> m (a, Map k v)
-- runUnsafeMapStore = runStateT

-- -- | A 'Store' implemented using a @Map k v@. Falls back to the default value
-- -- computed by an enclosed @k -> v@ function when accessing a non-existent key.
-- type MapStore m k v = ReaderT (k -> v) (UnsafeMapStore m k v)

-- instance (Ord k, Monad m) => Store (MapStore m k v) k v where
--     getValue k = do
--         f <- ask
--         findWithDefault (f k) k <$> lift get
--     putValue k = lift . putValue k

-- -- | Run a 'MapStore' computation on a given initial state of the store.
-- -- Falls back to the default value computed by the provided @k -> v@ function
-- -- when accessing a non-existent key.
-- runMapStoreT :: MapStore m k v a -> (k -> v) -> Map k v -> m (a, Map k v)
-- runMapStoreT store = runStateT . runReaderT store

-- runMapStore :: MapStore Identity k v a -> (k -> v) -> Map k v -> (a, Map k v)
-- runMapStore store defaultValue = runIdentity . runStateT (runReaderT store defaultValue)
