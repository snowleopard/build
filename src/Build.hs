{-# LANGUAGE ConstraintKinds, RankNTypes, TypeApplications #-}

-- | Build systems and the properties they should ensure.
module Build (
    -- * Build
    Build,

    -- * Properties
    correct, correctBuild, idempotent
    ) where

import Build.Task
import Build.Task.Monad
import Build.Store
import Build.Utilities

-- | A build system takes a description of 'Tasks', a target key, and a store,
-- and computes a new store, where the key and its dependencies are up to date.
type Build c i k v = Tasks c k v -> k -> Store i k v -> Store i k v

-- | Given a description of @tasks@, an initial @store@, and a @result@ produced
-- by running a build system on a target @key@, this function returns 'True' if
-- the @result@ is a correct build outcome. Specifically:
-- * @result@ and @store@ must agree on the values of all inputs. In other words,
--   no inputs were corrupted during the build.
-- * @result@ is /consistent/ with the @tasks@, i.e. for every non-input key,
--   the result of recomputing its task matches the value stored in the @result@.
correctBuild :: (Ord k, Eq v) => Tasks Monad k v -> Store i k v -> Store i k v -> k -> Bool
correctBuild tasks store result = all correct . reachable deps
  where
    deps = maybe [] (\task -> snd $ track task (flip getValue result)) . tasks
    correct k = case tasks k of
        Nothing   -> getValue k result == getValue k store
        Just task -> getValue k result == compute task (flip getValue result)

-- | Given a @build@ and @tasks@, check that @build@ produces a correct result
-- for any initial store and a target key.
correct :: (Ord k, Eq v) => Build Monad i k v -> Tasks Monad k v -> Bool
correct build tasks = forall $ \(key, store) ->
    correctBuild tasks store (build tasks key store) key

-- | Check that a build system is /idempotent/, i.e. running it once or twice in
-- a row leads to the same resulting 'Store'.
idempotent :: Eq v => Build Monad i k v -> Tasks Monad k v -> Bool
idempotent build tasks = forall $ \(key, store1) ->
    let store2 = build tasks key store1
        store3 = build tasks key store2
    in forall $ \k -> getValue k store2 == getValue k store3
