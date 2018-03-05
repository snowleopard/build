{-# LANGUAGE ConstraintKinds, RankNTypes, ScopedTypeVariables #-}
module Development.Build (
    -- * Build
    Build, PureBuild, purify, dumbBuild, dumbTracingBuild, slowBuild,

    -- * Properties
    correct, idempotent
    ) where

import Control.Monad.IO.Class
import Data.Functor
import Data.Functor.Identity

import Development.Build.Compute
import Development.Build.Compute.Monad
import Development.Build.Store
import Development.Build.Utilities

-- | A build system takes a 'Compute', a list of output keys, an initial state
-- of type @s@, and builds the outputs, producing the new state.
type Build c m k v s =
    Store m k v => Compute c k v -> [k] -> s -> m s

type PureBuild c k v s = Compute c k v -> [k] -> (s, k -> v) -> (s, k -> v)

-- How do I express that we can do it for any c, not just c = Monad?
purify :: Eq k => (forall m. Build Monad m k v s) -> PureBuild Monad k v s
purify build compute outputs (s, store) =
    runIdentity $ runPureStore (build compute outputs s) store

dumbBuild :: Build Monad m k v ()
dumbBuild compute outputs () = mapM build outputs $> ()
  where
    build k = do
        maybeValue <- compute getValue k
        case maybeValue of
            Just value -> putValue k value
            Nothing    -> return ()

dumbTracingBuild :: (MonadIO m, Show k, Show v) => Build Monad m k v ()
dumbTracingBuild compute outputs () = mapM build outputs $> ()
  where
    build k = do
        let myGetValue k = do
                v <- getValue k
                liftIO $ putStrLn $ "Looked up key: " ++ show k ++ " => " ++ show v
                return v
        liftIO $ putStrLn ("Computing key: " ++ show k)
        maybeValue <- compute myGetValue k
        case maybeValue of
            Just value -> putValue k value
            Nothing    -> return ()

slowBuild :: Build Monad m k v ()
slowBuild compute outputs s = mapM build outputs $> s
  where
    build k = do
        -- This is an incorrect heuristic
        let ready = null (staticDependencies compute k)
        maybeValue <- compute (if ready then getValue else build) k
        case maybeValue of
            Just value -> putValue k value >> return value
            Nothing    -> getValue k

-- | Given a @build@ and @compute@, check that for any key-value map describing
-- the contents of a store @before@ the build system is executed to build a list
-- of @outputs@, the map @after@ the build is a correct build outcome.
-- Specifically, there must exist a @magic@ key-value map, such that:
-- * @before@, @after@ and @magic@ agree on the values of all inputs.
-- * @after@ and @magic@ agree on the values of all outputs.
-- * @magic@ is 'consistent' with the @compute@.
-- We assume that @compute@ is acyclic. If it is not, the function returns @True@.
correct :: (Eq k, Eq v) => (forall m. Build Monad m k v s)
                        -> Compute Monad k v -> Bool
correct build compute = forall $ \(outputs, store, s) ->
    let (_, newStore) = (purify build) compute outputs (s, store)
    in correctBuild compute store newStore outputs

-- | Check that a build system is /idempotent/, i.e. running it once or twice in
-- a row leads to the same resulting 'Store'.
idempotent :: (Eq k, Eq v) => (forall m. Build Monad m k v s)
                           -> Compute Monad k v -> Bool
idempotent build compute = forall $ \(outputs, store1, s1) ->
    let (s2, store2) = (purify build) compute outputs (s1, store1)
        (_ , store3) = (purify build) compute outputs (s2, store2)
    in forall $ \key -> store2 key == store3 key
