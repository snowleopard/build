{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Build.Trace (
    -- * Verifying traces
    VT, recordVT, verifyVT,

    -- * Constructive traces
    CT, recordCT, verifyCT, constructCT,

    -- * Constructive traces optimised for deterministic tasks
    CTD, recordCTD, verifyCTD, constructCTD
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
recordVT :: Hashable v => Store i k v -> k -> [k] -> VT k v
recordVT store key deps = VT [Trace key [ (k, getHash k store) | k <- deps ] (getHash key store)]

-- | Given a function to compute the hash of a key's current value,
-- a @key@, and a set of verifying traces, return 'True' if the @key@ is
-- up-to-date.
verifyVT :: (Monad m, Eq k) => (k -> m (Hash v)) -> k -> VT k v -> m Bool
verifyVT fetchHash key (VT ts) = anyM match ts
  where
    match (Trace k deps result)
        | k /= key  = return False
        | otherwise = andM [ (h==) <$> fetchHash k | (k, h) <- (key, result) : deps ]

data CT k v = CT
    { traces    :: VT k v
    , contents  :: Map (Hash v) v }


instance Semigroup (CT k v) where
    CT t1 c1 <> CT t2 c2 = CT (t1 <> t2) (Map.union c1 c2)

instance Monoid (CT k v) where
    mempty  = CT mempty Map.empty
    mappend = (<>)

recordCT :: Hashable v => Store i k v -> k -> [k] -> CT k v
recordCT store key deps = CT (recordVT store key deps) (Map.singleton (getHash key store) (getValue key store))

verifyCT :: (Monad m, Eq k) => (k -> m (Hash v)) -> k -> CT k v -> m Bool
verifyCT fetchHash key (CT ts _) = verifyVT fetchHash key ts

constructCT :: (Monad m, Eq k) => (k -> m (Hash v)) -> k -> CT k v -> m (Maybe v)
constructCT fetchHash key (CT (VT ts) cache) = firstJustM match ts
  where
    match (Trace k deps result) = do
        sameInputs <- andM $ return (k == key) : [ (h==) <$> fetchHash k | (k, h) <- deps ]
        if not sameInputs
            then return Nothing
            else return (Map.lookup result cache)

newtype CTD k v = CTD (Map (Hash (k, [v])) v)

instance Semigroup (CTD k v) where
    CTD c1 <> CTD c2 = CTD (Map.union c1 c2)

instance Monoid (CTD k v) where
    mempty  = CTD Map.empty
    mappend = (<>)

recordCTD :: (Hashable k, Hashable v) => Store i k v -> k -> [k] ->CTD k v
recordCTD store key deps = CTD (Map.singleton h (getValue key store))
  where
    h = hash (key, map (flip getValue store) deps)

verifyCTD :: (Eq v, Hashable k, Hashable v) => Store i k v -> k -> [k] -> CTD k v -> Bool
verifyCTD store key deps ctd = Just (getValue key store) == constructCTD store key deps ctd

constructCTD :: (Hashable k, Hashable v) => Store i k v -> k -> [k] -> CTD k v -> Maybe v
constructCTD store key deps (CTD cache) = Map.lookup h cache
  where
    h = hash (key, map (flip getValue store) deps)
