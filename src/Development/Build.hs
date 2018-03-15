{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes #-}
module Development.Build (
    -- * Build
    Build, dumb, busy, memo,

    -- * MultiBuild
    MultiBuild, sequentialMultiBuild,

    -- * Properties
    correct, idempotent
    ) where

import Control.Monad.State

import Development.Build.Task
import Development.Build.Task.Monad
import Development.Build.Store
import Development.Build.Utilities

-- | A build system takes a 'Task', a key to build, some information from
-- the previous build @i@ (which can be missing if this is the first build),
-- and a key-value map @k -> v@, and computes information for the next build and
-- an updated key-value map. Note that we require @Eq k@ since without it one
-- has no way of updating the map.
type Build c i k v = Task c k v -> k -> Store i k v -> Store i k v

dumb :: (Eq k, Hashable v) => Build Monad i k v
dumb task key store = case execute task (getValue store) key of
    Nothing    -> store
    Just value -> putValue store key value

busy ::(Eq k, Hashable v) => Build Monad () k v
busy task key store = execState (compute key) store
  where
    -- compute :: k -> State (Store () k v) v
    compute k = case task compute k of
        Nothing  -> do { s <- get; return (getValue s k) }
        Just act -> do { v <- act; modify (\s -> putValue s k v); return v }

memo :: (Eq k, Hashable v) => Build Monad () k v
memo task key store = fst $ execState (compute key) (store, [])
  where
    -- compute :: k -> State (Store () k v, [k]) v
    compute k = case task compute k of
        Nothing  -> do { s <- fst <$> get; return (getValue s k) }
        Just act -> do
            built <- snd <$> get
            when (k `notElem` built) $ do
                v <- act
                modify $ \(s, built) -> (putValue s k v, k : built)
            s <- fst <$> get
            return (getValue s k)

type MultiBuild c i k v = Task c k v -> [k] -> Store i k v -> Store i k v

sequentialMultiBuild :: Build Monad i k v -> MultiBuild Monad i k v
sequentialMultiBuild build task outputs store = case outputs of
    []     -> store
    (k:ks) -> sequentialMultiBuild build task ks (build task k store)

-- | Given a @build@ and @task@, check that for any key-value map describing
-- the contents of a store @before@ the build system is executed to build a list
-- of @outputs@, the map @after@ the build is a correct build outcome.
-- Specifically, there must exist a @magic@ key-value map, such that:
-- * @before@, @after@ and @magic@ agree on the values of all inputs.
-- * @after@ and @magic@ agree on the values of all outputs.
-- * @magic@ is 'consistent' with the @task@.
-- We assume that @task@ is acyclic. If it is not, the function returns @True@.
correct :: (Eq k, Eq v) => Build Monad i k v -> Task Monad k v -> Bool
correct build task = forall $ \(key, store) ->
    correctBuild task store (build task key store) key

-- TODO: Switch to getHash
-- | Check that a build system is /idempotent/, i.e. running it once or twice in
-- a row leads to the same resulting 'Store'.
idempotent :: Eq v => Build Monad i k v -> Task Monad k v -> Bool
idempotent build task = forall $ \(key, store1) ->
    let store2 = build task key store1
        store3 = build task key store2
    in forall $ \k -> getValue store2 k == getValue store3 k
