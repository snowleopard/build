{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Development.Build.Store (
    -- * Basic types
    Hash, hash,

    -- * Store manipulation
    Store, getValue, setValue, setValues, getHash, unsafeMapStore, mapStore,

    -- * Properties
    consistent
    ) where

import Data.Map

import Development.Build.Utilities

-- | A 'Hash' is used for efficient tracking and sharing of build results. We
-- use @newtype Hash v = Hash v@ for prototyping.
newtype Hash v = Hash v deriving (Eq, Show)

-- | Compute the hash of a given value. We typically assume cryptographic
-- hashing, e.g. SHA256. Here we use @hash v = Hash v@ for prototyping.
hash :: v -> Hash v
hash = Hash

-- | A key-value store, which in addition to usual 'getValue' and 'setValue'
-- queries supports @getHash store key@, which is required to be equivalent to
-- @hash (getValue store key)@ but can in some cases be implemented more
-- efficiently. For example, GVFS (Git Virtual File System) downloads actual
-- values (file contents) only on demand. Note that if a file does not exist,
-- the corresponding 'file not found' value (suitably encoded) is still useful
-- and can be tracked by a build system. See 'consistent' for the list of
-- invariants that must be satisfied by a 'Store'.
data Store k v = forall s. Store s (s -> k -> v) (k -> v -> s -> s) (s -> k -> Hash v)

-- | Lookup the value of a key in a 'Store'.
getValue :: Store k v -> k -> v
getValue (Store s f _ _) = f s

-- | Modify a 'Store' by updating a key-value entry.
setValue :: k -> v -> Store k v -> Store k v
setValue key value (Store s f g h) = Store (g key value s) f g h

-- | Modify a 'Store' by updating a list of key-value entries.
setValues :: [(k, v)] -> Store k v -> Store k v
setValues []             = id
setValues ((k, v) : kvs) = setValues kvs . setValue k v

-- | Lookup the hash of a key in a 'Store'.
getHash :: Store k v -> k -> Hash v
getHash (Store s _ _ h) = h s

-- | A 'Store' implemented using a @Map k v@. Throws an error when accessing a
-- non-existent key.
unsafeMapStore :: Ord k => Store k v
unsafeMapStore = Store empty (!) insert ((hash .) . (!))

-- | A 'Store' implemented using a @Map k v@. Falls back to the @defaultValue key@
-- when accessing a non-existent key.
mapStore :: Ord k => (k -> v) -> Store k v
mapStore defaultValue = Store empty (!!) insert ((hash .) . (!!))
  where
    s !! k = findWithDefault (defaultValue k) k s

instance Eq v => Eq (Store k v) where
    s1 == s2 = forall $ \key -> getValue s1 key == getValue s2 key

-- | A store is consistent if it satisfies 4 invariants:
-- * Get after set: @getValue (setValue k v s) k == v@.
-- * Set after get: @setValue k (getValue s k) s == s@.
-- * Set after set: @setValue k v (setValue k v' s) s == setValue k v s@.
-- * Hash is consitent: @hash (getValue s k) == getHash s k@.
consistent :: forall k v. Eq v => Store k v -> Bool
consistent store = forall $ \(key, value, value' :: v) ->
    -- Get after set
    getValue (setValue key value store) key == value
    &&
    -- Set after get
    setValue key (getValue store key) store == store
    &&
    -- Set after set
    setValue key value (setValue key value' store) == setValue key value store
    &&
    -- Hash is consitent
    hash (getValue store key) == getHash store key
