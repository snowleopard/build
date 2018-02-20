{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables #-}
module Development.Build.Plan where

import Data.String
import System.FilePath

import Development.Build.Store
import Development.Build.Utilities

-- | A build plan, i.e. a partial map from a 'Key' to the 'Hash' of its 'Value',
-- plus a list of its ('Key', 'Hash') dependencies. Build plans are typically
-- reused from one build to the next to avoid rediscovering build dependencies
-- from scratch. Note that builds do not always go according to plan, and the
-- list of dependencies may need to be recomputed during the build. A key is
-- considered to be an /input/ if the list of its dependencies is empty, i.e. if
-- @plan key == Just (h, [])@. If @plan key == Nothing@, we don't know anything
-- about the key and if it is required for building any of the /outputs/ the
-- build system will need to discover its dependencies. We require that every
-- plan is 'acyclic'.
type Plan = Key -> Maybe (Hash, [(Key, Hash)])

-- | Example build plan containing information only for a single file:
-- @"f.o" -> Just (hash "1", [("f.c", hash "2"), ("gcc.exe", hash "3")])@.
examplePlan :: Plan
examplePlan key = case key of
    "f.o" -> Just (hash "1", [("f.c", hash "2"), ("gcc.exe", hash "3")])
    _     -> Nothing

-- | Sometimes you have no plan at all, i.e. @emptyPlan = const Nothing@.
emptyPlan :: Plan
emptyPlan = const Nothing

-- | Check that a given 'Plan' has no cyclic dependencies.
acyclic :: Plan -> Bool
acyclic plan = forall $ \key -> key `notElem` dependencies key
  where
    dependencies k = case plan k of
        Nothing        -> []
        Just (_, deps) -> concatMap (dependencies . fst) deps

-- | Find the inputs of a 'Key' that are listed in a given 'Plan'. Note that
-- since the plan can be incomplete, the result may be a subset of the actual
-- set of inputs.
inputs :: Plan -> Key -> [Key]
inputs plan key = case plan key of
    Nothing -> [] -- If the plan is incomplete, we return an underapproximation
    Just (_, []  ) -> [key] -- This key has no dependencies, so it is an input
    Just (_, deps) -> concat [ inputs plan k | (k, _) <- deps ]
