{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Build.Trace (
    -- * Verifying traces
    VT, recordVT, verifyVT,

    -- * Constructive traces
    CT, recordCT, verifyCT, constructCT,

    Trace (..), traceMatch, Traces (..)
    ) where

import Build.Store

import Control.Monad.Extra
import Data.Map (Map)
import Data.Semigroup

import qualified Data.Map as Map

-- | An abstract data type for a set of verifying traces equipped with 'record',
-- 'verify' and a 'Monoid' instance.
newtype VT k v = VT [Trace k v] deriving (Monoid, Semigroup)

-- | Record a new trace for building a @key@ with dependencies @deps@, obtaining
-- the hashes of up-to-date values from the given @store@.
recordVT :: Hashable v => Store i k v -> k -> [k] -> VT k v -> VT k v
recordVT store key deps (VT ts) = VT (t : ts)
  where
    t = Trace key [ (k, getHash k store) | k <- deps ] (getHash key store)

-- | Given a function to compute the hash of a key's current value,
-- a @key@, and a set of verifying traces, return 'True' if the @key@ is
-- up-to-date.
verifyVT :: (Monad m, Eq k) => (k -> m (Hash v)) -> k -> VT k v -> m Bool
verifyVT fetchHash key (VT ts) = anyM match ts
  where
    match (Trace k deps result)
        | k /= key  = return False
        | otherwise = andM [ (h==) <$> fetchHash k | (k, h) <- (key, result) : deps ]

newtype CT k v = CT (Traces k v)

instance Semigroup (CT k v) where
    CT (Traces t1 c1) <> CT (Traces t2 c2) = CT (Traces (t1 ++ t2) (Map.union c1 c2))

instance Monoid (CT k v) where
    mempty  = CT (Traces [] Map.empty)
    mappend = (<>)

recordCT :: Hashable v => Store i k v -> k -> [k] -> CT k v -> CT k v
recordCT store key deps (CT (Traces ts c)) = CT (Traces (t : ts) (Map.insert h v c))
  where
    h = getHash key store
    v = getValue key store
    t = Trace key [ (k, getHash k store) | k <- deps ] h

verifyCT :: (Monad m, Eq k) => (k -> m (Hash v)) -> k -> CT k v -> m Bool
verifyCT fetchHash key (CT (Traces ts _)) = verifyVT fetchHash key (VT ts)

constructCT :: (Monad m, Eq k) => (k -> m (Hash v)) -> k -> CT k v -> m (Maybe v)
constructCT fetchHash key (CT (Traces ts cache)) = firstJustM match ts
  where
    match (Trace k deps result) = do
        sameInputs <- andM [ (h==) <$> fetchHash k | (k, h) <- deps ]
        if (k /= key || not sameInputs)
            then return Nothing
            else return (Map.lookup result cache)

data Traces k v = Traces
    { traces :: [Trace k v]
    , contents  :: Map (Hash v) v }

data Trace k v = Trace
    { key     :: k
    , depends :: [(k, Hash v)]
    , result  :: Hash v }

-- Determine whether a trace is relevant to the current state
traceMatch :: (Monad m, Eq k) => (k -> Hash v -> m Bool) -> k -> [Trace k v] -> m [Hash v]
traceMatch check key ts = mapMaybeM f ts
  where
    f (Trace k dkv h) = do
        b <- return (key == k) &&^ allM (uncurry check) dkv
        return $ if b then Just h else Nothing
