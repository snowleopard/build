{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes #-}
module Development.Build (
    -- * Build
    Build, MultiBuild, sequentialMultiBuild, --, sequentialMultiStoreBuild,
    dumb, busy, -- purify, slow, dumbTracing,

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

type MultiBuild c i k v = Task c k v -> [k] -> Store i k v -> Store i k v

-- type StoreBuild c m k v = Store m k v => Task c k v -> k -> m ()

-- type MultiStoreBuild c m k v = Store m k v => Task c k v -> [k] -> m ()

-- How do I express that we can do it for any c, not just c = Monad?
-- purify :: (forall m. StoreBuild Monad m k v) -> Build Monad () k v
-- purify build task key _ = runState (build task key)

sequentialMultiBuild :: Build Monad i k v -> MultiBuild Monad i k v
sequentialMultiBuild build task outputs store = case outputs of
    []     -> store
    (k:ks) -> sequentialMultiBuild build task ks (build task k store)

-- sequentialMultiStoreBuild :: StoreBuild Monad m k v -> MultiStoreBuild Monad m k v
-- sequentialMultiStoreBuild build task = mapM_ (build task)

dumb :: (Eq k, Hashable v) => Build Monad i k v
dumb task key store = case execute task (getValue store) key of
    Nothing    -> store
    Just value -> putValue store key value

-- dumbTracing :: (MonadIO m, Show k, Show v) => StoreBuild Monad m k v
-- dumbTracing task = build
--   where
--     build k = do
--         let myGetValue k = do
--                 v <- getValue k
--                 liftIO $ putStrLn $ "Looked up key: " ++ show k ++ " => " ++ show v
--                 return v
--         liftIO $ putStrLn ("Computing key: " ++ show k)
--         mapM_ (putValue k =<<) (task myGetValue k)

busy ::(Eq k, Hashable v) => Build Monad () k v
busy task key store = execState (go key) store
  where
    -- go :: k -> State (Store () k v) v
    go k = case task go k of
        Nothing  -> do { s <- get; return (getValue s k) }
        Just act -> do { v <- act; modify (\s -> putValue s k v); return v }

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
