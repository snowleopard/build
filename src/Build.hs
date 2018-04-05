{-# LANGUAGE ConstraintKinds, RankNTypes #-}
module Build (
    -- * Build
    Build,

    -- * MultiBuild
    MultiBuild, sequentialMultiBuild, sequentialMultiBuildA,

    -- * Properties
    correct, idempotent
    ) where

import Build.Task
import Build.Task.Monad hiding (dependencies)
import Build.Store
import Build.Utilities

-- | A build system takes a 'Task', a key to build, some information from
-- the previous build @i@ (which can be missing if this is the first build),
-- and a key-value map @k -> v@, and computes information for the next build and
-- an updated key-value map. Note that we require @Eq k@ since without it one
-- has no way of updating the map.
type Build c i k v = Tasks c k v -> k -> Store i k v -> Store i k v

type MultiBuild c i k v = Tasks c k v -> [k] -> Store i k v -> Store i k v

sequentialMultiBuild :: Build Monad i k v -> MultiBuild Monad i k v
sequentialMultiBuild build task outputs store = case outputs of
    []     -> store
    (k:ks) -> sequentialMultiBuild build task ks (build task k store)

sequentialMultiBuildA :: Build Applicative i k v -> MultiBuild Applicative i k v
sequentialMultiBuildA build task outputs store = case outputs of
    []     -> store
    (k:ks) -> sequentialMultiBuildA build task ks (build task k store)

-- | Given a @build@ and @task@, check that for any key-value map describing
-- the contents of a store @before@ the build system is executed to build a list
-- of @outputs@, the map @after@ the build is a correct build outcome.
-- Specifically, there must exist a @magic@ key-value map, such that:
-- * @before@, @after@ and @magic@ agree on the values of all inputs.
-- * @after@ and @magic@ agree on the values of all outputs.
-- * @magic@ is 'consistent' with the @task@.
-- We assume that @task@ is acyclic. If it is not, the function returns @True@.
correct :: (Ord k, Eq v) => Build Monad i k v -> Tasks Monad k v -> Bool
correct build tasks = forall $ \(key, store) ->
    correctBuild tasks store (build tasks key store) key

-- TODO: Switch to getHash
-- | Check that a build system is /idempotent/, i.e. running it once or twice in
-- a row leads to the same resulting 'Store'.
idempotent :: Eq v => Build Monad i k v -> Tasks Monad k v -> Bool
idempotent build tasks = forall $ \(key, store1) ->
    let store2 = build tasks key store1
        store3 = build tasks key store2
    in forall $ \k -> getValue k store2 == getValue k store3
