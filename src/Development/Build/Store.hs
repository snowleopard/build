{-# LANGUAGE FlexibleInstances, DefaultSignatures, FunctionalDependencies #-}
module Development.Build.Store (
    -- * Hashing
    Hash, hash,

    -- * The store monad
    Get (..), GetHash (..), Put (..), checkHashes, need,
    UnsafeMapStore, runUnsafeMapStore, MapStore, runMapStore
    ) where

import Data.Map
import Control.Monad.State
import Control.Monad.Trans.Reader

-- | A 'Hash' is used for efficient tracking and sharing of build results. We
-- use @newtype Hash v = Hash v@ for prototyping.
newtype Hash v = Hash v deriving (Eq, Show)

-- | Compute the hash of a given value. We typically assume cryptographic
-- hashing, e.g. SHA256. Here we use @hash v = Hash v@ for prototyping.
hash :: v -> Hash v
hash = Hash

class Get f k v | f -> k v where
    getValue :: k -> f v

class Get f k v => GetHash f k v | f -> k v where
    getHash :: k -> f (Hash v)
    default getHash :: Functor f => k -> f (Hash v)
    getHash = fmap hash . getValue

class Put f k v | f -> k v where
    putValue :: k -> v -> f ()

-- | A key-value store monad that in addition to usual 'getValue' and 'putValue'
-- queries supports 'getHash', which can in some cases be implemented more
-- efficiently than by hashing the result of 'getValue'. For example, GVFS (Git
-- Virtual File System) downloads actual values (file contents) only on demand.
-- Note that if a file does not exist, the corresponding 'file not found' value
-- (suitably encoded) is still useful and can be tracked by a build system.

-- | Depend on a given key. Convenient for registering dependencies of external
-- build tools that are not passed as explicit values.
need :: (Monad m, Get m k v) => [k] -> m ()
need = mapM_ getValue

checkHashes :: (Eq v, Monad m, GetHash m k v) => [(k, Hash v)] -> m Bool
checkHashes khs = do
    let (ks, hs) = unzip khs
    storedHashes <- mapM getHash ks
    return $ hs == storedHashes

-- | A 'Store' implemented using a @Map k v@. Throws an error when accessing a
-- non-existent key. See 'MapStore' for a safe alternative.
type UnsafeMapStore m k v = StateT (Map k v) m

-- | Run an 'UnsafeMapStore' computation on a given initial state of the store,
-- Throws an error when accessing a non-existent key. See 'runMapStore' for a
-- safe alternative.
runUnsafeMapStore :: UnsafeMapStore m k v a -> Map k v -> m (a, Map k v)
runUnsafeMapStore = runStateT

instance (Monad m, Ord k) => Get (UnsafeMapStore m k v) k v where
    getValue k = (!k) <$> get

instance (Monad m, Ord k) => Put (UnsafeMapStore m k v) k v where
    putValue k v = modify (insert k v)

-- | A 'Store' implemented using a @Map k v@. Falls back to the default value
-- computed by an enclosed @k -> v@ function when accessing a non-existent key.
type MapStore m k v = ReaderT (k -> v) (UnsafeMapStore m k v)

-- | Run a 'MapStore' computation on a given initial state of the store.
-- Falls back to the default value computed by the provided @k -> v@ function
-- when accessing a non-existent key.
runMapStore :: MapStore m k v a -> (k -> v) -> Map k v -> m (a, Map k v)
runMapStore store = runStateT . runReaderT store

instance (Monad m, Ord k) => Get (MapStore m k v) k v where
    getValue k = do
        f <- ask
        findWithDefault (f k) k <$> lift get

instance (Monad m, Ord k) => Put (MapStore m k v) k v where
    putValue k = lift . putValue k
