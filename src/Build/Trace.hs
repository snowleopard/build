{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Build.Trace (
    -- * Verifying traces
    VT, recordVT, verifyVT,

    -- * Constructive traces
    CT, recordCT, constructCT,

    -- * Constructive traces optimised for deterministic tasks
    DCT, recordDCT, verifyDCT, constructDCT
    ) where

import Build.Store

import Control.Monad.Extra
import Data.Map (Map)
import Data.Maybe
import Data.Semigroup

import qualified Data.Map as Map

-- A trace is parameterised by the types of keys @k@, hashes @h@, as well as the
-- result @r@. For verifying traces, @r = h@; for constructive traces, @Hash r = h@.
data Trace k h r = Trace
    { key     :: k
    , depends :: [(k, h)]
    , result  :: r }

-- | An abstract data type for a set of verifying traces equipped with 'record',
-- 'verify' and a 'Monoid' instance.
newtype VT k v = VT [Trace k (Hash v) (Hash v)] deriving (Monoid, Semigroup)

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

newtype CT k v = CT [Trace k (Hash v) v] deriving (Monoid, Semigroup)

recordCT :: Monad m => k -> v -> [k] -> (k -> m (Hash v)) -> m (CT k v)
recordCT key value deps fetchHash = do
    hs <- mapM fetchHash deps
    return $ CT ([Trace key (zip deps hs) value])

-- Prefer constructing the currenct value, if it matches one of the traces.
constructCT :: (Monad m, Eq k, Ord v) => k -> v -> (k -> m (Hash v)) -> CT k v -> m (Maybe v)
constructCT key value fetchHash (CT ts) = do
    candidates <- catMaybes <$> mapM match ts
    if value `elem` candidates then return $ Just value
                               else return $ listToMaybe candidates
  where
    match (Trace k deps result)
        | k /= key  = return Nothing
        | otherwise = do
            sameInputs <- andM [ (h==) <$> fetchHash k | (k, h) <- deps ]
            return $ if sameInputs then Just result else Nothing

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
