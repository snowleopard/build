{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Build.Trace (
    -- * Verifying traces
    VT, recordVT, verifyVT,

    -- * Constructive traces
    CT, recordCT, verifyCT, constructCT,

    -- * Constructive traces optimised for deterministic tasks
    DCT, recordDCT, verifyDCT, constructDCT
    ) where

import Build.Store

import Control.Monad.Extra
import Data.Map (Map)
import Data.Semigroup

import qualified Data.Map as Map

data Trace k v = Trace
    { key     :: k
    , depends :: [(k, Hash v)]
    , result  :: Hash v }

-- | An abstract data type for a set of verifying traces equipped with 'record',
-- 'verify' and a 'Monoid' instance.
newtype VT k v = VT [Trace k v] deriving (Monoid, Semigroup)

-- | Record a new trace for building a @key@ with dependencies @deps@, obtaining
-- the hashes of up-to-date values from the given @store@.
recordVT :: (Hashable v, Monad m) => k -> v -> [k] -> (k -> m (Hash v)) -> m (VT k v)
recordVT key value deps fetchHash = do
    hs <- mapM fetchHash deps
    return $ VT [ Trace key (zip deps hs) (hash value) ]

-- | Given a function to compute the hash of a key's current value,
-- a @key@, and a set of verifying traces, return 'True' if the @key@ is
-- up-to-date.
verifyVT :: (Monad m, Eq k, Hashable v) => k -> v -> (k -> m (Hash v)) -> VT k v -> m Bool
verifyVT key value fetchHash (VT ts) = anyM match ts
  where
    match (Trace k deps result)
        | k /= key || result /= hash value = return False
        | otherwise = andM [ (h==) <$> fetchHash k | (k, h) <- deps ]

data CT k v = CT
    { traces    :: VT k v
    , contents  :: Map (Hash v) v }

instance Ord v => Semigroup (CT k v) where
    CT t1 c1 <> CT t2 c2 = CT (t1 <> t2) (Map.union c1 c2)

instance Ord v => Monoid (CT k v) where
    mempty  = CT mempty Map.empty
    mappend = (<>)

recordCT :: (Hashable v, Monad m) => k -> v -> [k] -> (k -> m (Hash v)) -> m (CT k v)
recordCT key value deps fetchHash = do
    hs <- mapM fetchHash deps
    let h = hash value
    return $ CT (VT [Trace key (zip deps hs) h]) (Map.singleton h value)

verifyCT :: (Monad m, Eq k, Hashable v) => k -> v -> (k -> m (Hash v)) -> CT k v -> m Bool
verifyCT key value fetchHash (CT ts _) = verifyVT key value fetchHash ts

constructCT :: (Monad m, Eq k, Ord v) => k -> (k -> m (Hash v)) -> CT k v -> m (Maybe v)
constructCT key fetchHash (CT (VT ts) cache) = firstJustM match ts
  where
    match (Trace k deps result)
        | k /= key  = return Nothing
        | otherwise = do
            sameInputs <- andM [ (h==) <$> fetchHash k | (k, h) <- deps ]
            return $ if sameInputs then Map.lookup result cache else Nothing

newtype DCT k v = DCT (Map (Hash (k, [(k, Hash v)])) v)

instance (Ord k, Ord v) => Semigroup (DCT k v) where
    DCT c1 <> DCT c2 = DCT (Map.union c1 c2)

instance (Ord k, Ord v) => Monoid (DCT k v) where
    mempty  = DCT Map.empty
    mappend = (<>)

recordDCT :: (Hashable k, Hashable v, Monad m)
          => k -> v -> [k] -> (k -> m (Hash v)) -> m (DCT k v)
recordDCT key value deps fetchHash = do
    hs <- mapM fetchHash deps
    return $ DCT (Map.singleton (hash (key, zip deps hs)) value)

verifyDCT :: (Hashable k, Hashable v, Monad m)
          => k -> [k] -> (k -> m (Hash v)) -> DCT k v -> m Bool
verifyDCT key deps fetchHash ctd = do
    maybeValue <- constructDCT key deps fetchHash ctd
    case maybeValue of
        Nothing -> return False
        Just value -> (hash value ==) <$> fetchHash key

constructDCT :: (Hashable k, Hashable v, Monad m)
             => k -> [k] -> (k -> m (Hash v)) -> DCT k v -> m (Maybe v)
constructDCT key deps fetchHash (DCT cache) = do
    hs <- mapM fetchHash deps
    return (Map.lookup (hash (key, zip deps hs)) cache)
