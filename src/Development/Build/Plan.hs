module Development.Build.Plan (
    -- Traces
    Result, checkResult, VerifyingTrace, verify, ConstructiveTrace, construct,

    -- * Plan
    Plan, noPlan,

    -- * Properties
    dependencies, acyclic, upToDate, consistent
    ) where

import Data.Maybe
import Data.Map (Map)
import Development.Build.Store
import Development.Build.Utilities

import qualified Data.Map as Map

------------------------ New traces stuff (also wrong!) ------------------------

type Result k v = (Hash v, [(k, Hash v)])

checkResult :: Hashable v => Store i k v -> k -> Result k v -> Bool
checkResult store key (keyHash, deps) =
    and [ getHash store k == h | (k, h) <- (key, keyHash) : deps ]

type VerifyingTrace k v = Map k [Result k v]

verify :: (Ord k, Hashable v) => VerifyingTrace k v -> Store i k v -> k -> Bool
verify trace store key = case Map.lookup key trace of
    Nothing      -> False
    Just results -> any (checkResult store key) results

data ConstructiveTrace k v = ConstructiveTrace (VerifyingTrace k v) (Hash v -> v)

construct :: (Ord k, Hashable v) => ConstructiveTrace k v -> Store i k v -> k -> Maybe v
construct (ConstructiveTrace trace cache) store key
    | verify trace store key == False = Nothing
    | otherwise                       = Just $ cache (getHash store key)

------------------------ Old plan stuff (to be deleted) ------------------------

-- | A /build plan/ is a partial map from a key to the hash of its value, plus a
-- list of its @(key, hash)@ dependencies. Build plans are typically reused from
-- one build to the next to avoid rediscovering build dependencies from scratch.
-- Note that builds do not always go according to plan, and the list of
-- dependencies may need to be recomputed during the build. A key is considered
-- to be an /input/ if the list of its dependencies is empty, i.e. if
-- @plan key == Just (h, [])@. If @plan key == Nothing@, we don't know anything
-- about the key and if it is required for building any of the /outputs/ the
-- build system will need to discover its dependencies. We require that every
-- plan is 'acyclic'.
type Plan k v = k -> Maybe (Hash v, [(k, Hash v)])

-- | Sometimes you have no plan at all, i.e. @noPlan = const Nothing@.
noPlan :: Plan k v
noPlan = const Nothing

-- | Dependencies of a key according to a 'Plan', or @Nothing@ if the plan has
-- no information about the key.
dependencies :: Plan k v -> k -> Maybe [k]
dependencies plan key = map fst . snd <$> plan key

-- | Check that a given 'Plan' has no cyclic dependencies.
acyclic :: Eq k => Plan k v -> Bool
acyclic plan = forall $ \key -> isJust (reach knownDependencies key)
  where
    knownDependencies = concat . maybeToList . dependencies plan

-- | Check that according to a provided build 'Plan', a 'Store' contains an
-- /up-to-date/ value for a key.
--
-- * If there is no plan (@plan key == Nothing@) we conservatively assume that
--   the value is not up-to-date.
-- * Otherwise @plan key == Just (keyHash, deps)@, and we require that:
--
--     1. The value has expected hash, i.e. @getHash key@ returns @keyHash@.
--     2. All dependencies have expected hashes: i.e. @getHash k@ returns @h@
--        for each @(k, h)@ in @deps@.
upToDate :: Hashable v => Store i k v -> Plan k v -> k -> Bool
upToDate store plan key = case plan key of
    Nothing -> False -- We don't know and conservatively return False
    Just (keyHash, deps) -> checkHashes store ((key, keyHash) : deps)

-- | Check that a 'Plan' is consistent with respect to a given 'Store'.
-- * An empty plan (@plan key == Nothing@) is consistent.
-- * Otherwise @plan key == Just (keyHash, deps)@, and we require that:
--
--     1. The value has expected hash, i.e. @getHash key@ returns @keyHash@.
--     2. All dependencies have expected hashes: i.e. @getHash k@ returns @h@
--        for each @(k, h)@ in @deps@.
consistent :: Hashable v => Store i k v -> Plan k v -> Bool
consistent store plan = forall $ \key -> case plan key of
    Nothing -> True -- Incomplete plan is consistent
    Just (keyHash, deps) -> checkHashes store ((key, keyHash) : deps)
