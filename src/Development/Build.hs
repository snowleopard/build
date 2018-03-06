{-# LANGUAGE ConstraintKinds, RankNTypes #-}
module Development.Build (
    -- * Build
    Build, dumb, purify, slow, dumbTracing,

    -- * Properties
    correct, idempotent
    ) where

import Control.Monad.IO.Class
import Data.Functor
import Data.Map (Map)

import Development.Build.Compute
import Development.Build.Compute.Monad
import Development.Build.Store
import Development.Build.Utilities

import qualified Data.Map as Map

-- | A build system takes a 'Compute', a list of output keys, some information
-- from the previous build @i@ (which can be missing if this is the first build),
-- and a partial key-value map @Map k v@, and computes information for the next
-- build and an updated key-value map. Note that we require @Ord k@ since one
-- cannot do anything with 'Map' without it, but in principle @Eq k@ could be
-- sufficient if we instead used @k -> Maybe v@ for key-value maps.
type Build c i k v = Ord k => Compute c k v -> [k] -> Maybe i -> Map k v -> (i, Map k v)

type StoreBuild c m k v = Store m k v => Compute c k v -> [k] -> m ()

-- How do I express that we can do it for any c, not just c = Monad?
purify :: (k -> v) -> (forall m. StoreBuild Monad m k v) -> Build Monad () k v
purify def build compute outputs _ = runMapStore (build compute outputs) def

dumb :: (k -> v) -> Build Monad () k v
dumb def = purify def build
  where
    build compute outputs = mapM_ buildValue outputs
      where
        buildValue key = do maybeValue <- compute getValue key
                            case maybeValue of
                                Just value -> putValue key value
                                Nothing    -> return ()

dumbTracing :: (MonadIO m, Show k, Show v) => StoreBuild Monad m k v
dumbTracing compute outputs = mapM_ build outputs
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

slow :: (k -> v) -> Build Monad () k v
slow def = purify def build
  where
    build :: Store m k v => Compute Monad k v -> [k] -> m ()
    build compute outputs = void $ mapM buildValue outputs
      where
        buildValue key = do
            -- This is an incorrect heuristic
            let ready = null (staticDependencies compute key)
            maybeValue <- compute (if ready then getValue else buildValue) key
            case maybeValue of
                Just value -> putValue key value >> return value
                Nothing    -> getValue key

-- | Given a @build@ and @compute@, check that for any key-value map describing
-- the contents of a store @before@ the build system is executed to build a list
-- of @outputs@, the map @after@ the build is a correct build outcome.
-- Specifically, there must exist a @magic@ key-value map, such that:
-- * @before@, @after@ and @magic@ agree on the values of all inputs.
-- * @after@ and @magic@ agree on the values of all outputs.
-- * @magic@ is 'consistent' with the @compute@.
-- We assume that @compute@ is acyclic. If it is not, the function returns @True@.
correct :: (Ord k, Eq v) => Build Monad i k v -> Compute Monad k v -> Bool
correct build compute = forall $ \(outputs, store, i) ->
    let (_, newStore) = build compute outputs i store
    in correctBuild (partial compute) (total store) (total newStore) outputs
  where
    total :: Ord k => Map k v -> k -> Maybe v
    total = flip Map.lookup
    partial :: Monad m => Compute Monad k v -> (k -> m (Maybe v)) -> k -> m (Maybe (Maybe v))
    partial compute lookup = fmap (either (const Nothing) Just) . runPartial compute lookup

-- | Check that a build system is /idempotent/, i.e. running it once or twice in
-- a row leads to the same resulting 'Store'.
idempotent :: (Ord k, Eq v) => Build Monad i k v -> Compute Monad k v -> Bool
idempotent build compute = forall $ \(outputs, store1, s1) ->
    let (s2, store2) = build compute outputs (Just s1) store1
        (_ , store3) = build compute outputs (Just s2) store2
    in store2 == store3
