{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Development.Build.Store (
    -- * Hashing
    Hash, hash,

    -- * The store monad
    Store (..), need, checkHashes, UnsafeMapStore, runUnsafeMapStore, MapStore,
    runMapStore, Traced, trace
    ) where

import Data.Map
import Control.Monad.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

-- | A 'Hash' is used for efficient tracking and sharing of build results. We
-- use @newtype Hash v = Hash v@ for prototyping.
newtype Hash v = Hash v deriving (Eq, Show)

-- | Compute the hash of a given value. We typically assume cryptographic
-- hashing, e.g. SHA256. Here we use @hash v = Hash v@ for prototyping.
hash :: v -> Hash v
hash = Hash

-- | A key-value store monad that in addition to usual 'getValue' and 'setValue'
-- queries supports 'getHash', which can in some cases be implemented more
-- efficiently than by hashing the result of 'getValue'. For example, GVFS (Git
-- Virtual File System) downloads actual values (file contents) only on demand.
-- Note that if a file does not exist, the corresponding 'file not found' value
-- (suitably encoded) is still useful and can be tracked by a build system.
class Monad m => Store m k v | m -> k v where
    -- | Lookup the value of a key in a 'Store'.
    getValue :: k -> m v
    -- | Modify a 'Store' by updating a key-value entry.
    setValue :: k -> v -> m ()
    -- | Lookup the hash of a key in a 'Store'.
    getHash  :: k -> m (Hash v)
    getHash k = hash <$> getValue k

-- | Depend on a given key. Convenient for registering dependencies of external
-- build tools that are not passed as explicit values.
need :: Store m k v => [k] -> m ()
need = mapM_ getHash

checkHashes :: (Eq v, Store m k v) => [(k, Hash v)] -> m Bool
checkHashes khs = do
    let (ks, hs) = unzip khs
    storedHashes <- mapM getHash ks
    return $ hs == storedHashes

-- | A 'Store' implemented using a @Map k v@. Throws an error when accessing a
-- non-existent key. See 'MapStore' for a safe alternative.
type UnsafeMapStore k v = State (Map k v)

-- | Run an 'UnsafeMapStore' computation on a given initial state of the store,
-- Throws an error when accessing a non-existent key. See 'runMapStore' for a
-- safe alternative.
runUnsafeMapStore :: UnsafeMapStore k v a -> Map k v -> (a, Map k v)
runUnsafeMapStore = runState

instance Ord k => Store (UnsafeMapStore k v) k v where
    getValue k   = (!k) <$> get
    setValue k v = modify (insert k v)

-- | A 'Store' implemented using a @Map k v@. Falls back to the default value
-- computed by an enclosed @k -> v@ function when accessing a non-existent key.
type MapStore k v = ReaderT (k -> v) (UnsafeMapStore k v)

-- | Run a 'MapStore' computation on a given initial state of the store.
-- Falls back to the default value computed by the provided @k -> v@ function
-- when accessing a non-existent key.
runMapStore :: MapStore k v a -> (k -> v) -> Map k v -> (a, Map k v)
runMapStore store = runState . runReaderT store

instance Ord k => Store (MapStore k v) k v where
    getValue k = do
        f <- ask
        findWithDefault (f k) k <$> lift get
    setValue k = lift . setValue k

data TraceItem k v = GetValue k v | SetValue k v | GetHash k (Hash v)

type Traced m k v = WriterT [TraceItem k v] m

trace :: Store m k v => Traced m k v a -> m [TraceItem k v]
trace = execWriterT

instance Store m k v => Store (Traced m k v) k v where
    getValue k = do
        v <- lift (getValue k)
        tell [GetValue k v]
        return v
    setValue k v = do
        lift (setValue k v)
        tell [SetValue k v]
    getHash k = do
        h <- lift (getHash k)
        tell [GetHash k h]
        return h
