{-# LANGUAGE OverloadedStrings #-}
module Development.Build.Plan (
    -- * Plan
    Plan, examplePlan, emptyPlan,

    -- * Properties
    acyclic, upToDate, inputs, consistent
    ) where

import Development.Build.Store hiding (consistent)
import Development.Build.Utilities

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

-- | Example build plan containing information only for a single file:
-- @"f.o" -> Just (hash "1", [("f.c", hash "2"), ("gcc.exe", hash "3")])@.
examplePlan :: Plan FilePath String
examplePlan key = case key of
    "f.o" -> Just (hash "1", [("f.c", hash "2"), ("gcc.exe", hash "3")])
    _     -> Nothing

-- | Sometimes you have no plan at all, i.e. @emptyPlan = const Nothing@.
emptyPlan :: Plan k v
emptyPlan = const Nothing

-- | Check that a given 'Plan' has no cyclic dependencies.
acyclic :: Eq k => Plan k v -> Bool
acyclic plan = forall $ \key -> key `notElem` dependencies key
  where
    dependencies k = case plan k of
        Nothing        -> []
        Just (_, deps) -> concatMap (dependencies . fst) deps

-- | Check that according to a provided build 'Plan', a 'Store' contains an
-- /up-to-date/ value for a key.
--
-- * If there is no plan (@plan key == Nothing@) we conservatively assume that
--   the value is not up-to-date.
-- * Otherwise (@plan key == Just (h, deps)@) we require that:
--
--     1. The value has expected hash: @getHash store key == h@.
--     2. All dependencies have expected hashes: @getHash store key' == h'@ for
--        each @(key', h')@ in @deps@.
upToDate :: Eq v => Store k v -> Plan k v -> k -> Bool
upToDate store plan key = case plan key of
    Nothing        -> False -- We don't know and conservatively return False
    Just (h, deps) -> getHash store key == h
                   && and [ getHash store key' == h' | (key', h') <- deps ]

-- | Check that a 'Plan' is consistent with respect to a given 'Store'.
-- * An empty plan (@plan key == Nothing@) is consistent.
-- * Otherwise @plan key == Just (h, deps)@ and we require that:
--   1. @getHash store key == h@
--   2. @getHash store key' == h'@ for each @(key', h')@ in @deps@.
consistent :: Eq v => Plan k v -> Store k v -> Bool
consistent plan store = forall $ \key -> case plan key of
    Nothing -> True -- Incomplete plan is consistent
    Just (h, deps) -> getHash store key == h
                   && and [ getHash store key' == h' | (key', h') <- deps ]

-- | Find the inputs of a key that are listed in a given 'Plan'. Note that
-- since the plan can be incomplete, the result may be a subset of the actual
-- set of inputs.
inputs :: Plan k v -> k -> [k]
inputs plan key = case plan key of
    Nothing -> [] -- If the plan is incomplete, we return an underapproximation
    Just (_, []  ) -> [key] -- This key has no dependencies, so it is an input
    Just (_, deps) -> concat [ inputs plan k | (k, _) <- deps ]
