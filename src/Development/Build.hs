{-# LANGUAGE ConstraintKinds, RankNTypes #-}
module Development.Build (
    -- * Build
    Build, MultiBuild, sequentialMultiBuild, sequentialMultiStoreBuild,
    dumb, purify, slow, dumbTracing,

    -- * Properties
    correct, idempotent
    ) where

import Control.Monad.IO.Class
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
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
type Build c i k v = Ord k => Compute c k v -> k -> Maybe i -> Map k v -> (i, Map k v)

type MultiBuild c i k v = Ord k => Compute c k v -> NonEmpty k -> Maybe i -> Map k v -> (i, Map k v)

type StoreBuild c m k v = Store m k v => Compute c k v -> k -> m ()

type MultiStoreBuild c m k v = Store m k v => Compute c k v -> [k] -> m ()

-- How do I express that we can do it for any c, not just c = Monad?
purify :: (k -> v) -> (forall m. StoreBuild Monad m k v) -> Build Monad () k v
purify def build compute key _ = runMapStore (build compute key) def

sequentialMultiBuild :: Build Monad i k v -> MultiBuild Monad i k v
sequentialMultiBuild build compute (x :| xs) i store =
    let (i', store') = build compute x i store
    in case xs of
          []     -> (i', store')
          (y:ys) -> sequentialMultiBuild build compute (y :| ys) (Just i') store'

sequentialMultiStoreBuild :: StoreBuild Monad m k v -> MultiStoreBuild Monad m k v
sequentialMultiStoreBuild build compute = mapM_ (build compute)

dumb :: (k -> v) -> Build Monad () k v
dumb def = purify def $ \compute key ->
    mapM_ (putValue key =<<) (compute getValue key)

dumbTracing :: (MonadIO m, Show k, Show v) => StoreBuild Monad m k v
dumbTracing compute = build
  where
    build k = do
        let myGetValue k = do
                v <- getValue k
                liftIO $ putStrLn $ "Looked up key: " ++ show k ++ " => " ++ show v
                return v
        liftIO $ putStrLn ("Computing key: " ++ show k)
        mapM_ (putValue k =<<) (compute myGetValue k)

slow :: (k -> v) -> Build Monad () k v
slow def = purify def build
  where
    build compute = void . buildThenGet
      where
        buildThenGet k = case compute buildThenGet k of
            Nothing -> getValue k
            Just fv -> do
                value <- fv
                putValue k value
                return value

-- | Given a @build@ and @compute@, check that for any key-value map describing
-- the contents of a store @before@ the build system is executed to build a list
-- of @outputs@, the map @after@ the build is a correct build outcome.
-- Specifically, there must exist a @magic@ key-value map, such that:
-- * @before@, @after@ and @magic@ agree on the values of all inputs.
-- * @after@ and @magic@ agree on the values of all outputs.
-- * @magic@ is 'consistent' with the @compute@.
-- We assume that @compute@ is acyclic. If it is not, the function returns @True@.
correct :: (Ord k, Eq v) => Build Monad i k v -> Compute Monad k v -> Bool
correct build compute = forall $ \(key, store, i) ->
    let (_, newStore) = build compute key i store
    in correctBuild (partial compute) (lookup store) (lookup newStore) key
  where
    lookup :: Ord k => Map k v -> k -> Maybe v
    lookup = flip Map.lookup

-- | Check that a build system is /idempotent/, i.e. running it once or twice in
-- a row leads to the same resulting 'Store'.
idempotent :: (Ord k, Eq v) => Build Monad i k v -> Compute Monad k v -> Bool
idempotent build compute = forall $ \(outputs, store1, s1) ->
    let (s2, store2) = build compute outputs (Just s1) store1
        (_ , store3) = build compute outputs (Just s2) store2
    in store2 == store3
