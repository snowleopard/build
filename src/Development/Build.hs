{-# LANGUAGE ConstraintKinds, RankNTypes, ScopedTypeVariables #-}
module Development.Build (
    -- * Build
    Build, dumbBuild, dumbTracingBuild, slowBuild

    -- * Properties
    -- consistent, correct, idempotent
    ) where

import Control.Monad.IO.Class
import Data.Functor

import Development.Build.Compute
import Development.Build.Compute.Monad
import Development.Build.Store

-- | Check a three-way consistency between a 'Compute' function, a 'Plan' and
-- a 'Store' with respect to a given key. This involves checking the following:
-- * The plan is complete, i.e. all dependencies of the key are known.
-- * The ('Plan', 'Store') pair agrees with the 'Compute' function.
-- consistent :: (Eq v, Monad m, GetHash m k v) => Compute Monad k v -> Plan k v -> k -> m Bool
-- consistent compute plan key = case plan key of
--     Nothing -> return False -- The plan is incomplete
--     Just (h, deps) -> do
--         h' <- getHash key
--         vc <- compute key
--         vs <- getValue key
--         cs <- mapM (consistent compute plan . fst) deps
--         return $ h' == h && vc == vs && and cs

-- | A build system takes a 'Compute', a list of output keys, an initial state
-- of type @s@, and builds the outputs, producing the new state.
type Build c m k v s = (forall f. c f => Compute f k v) -> [k] -> s -> m s

dumbBuild :: Store m k v => Build Monad m k v ()
dumbBuild compute outputs () = mapM build outputs $> ()
  where
    build k = do
        maybeValue <- compute getValue k
        case maybeValue of
            Just value -> putValue k value
            Nothing    -> return ()

dumbTracingBuild :: (MonadIO m, Store m k v, Show k, Show v) => Build Monad m k v ()
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

slowBuild :: Store m k v => Build Monad m k v ()
slowBuild compute outputs s = mapM build outputs $> s
  where
    build k = do
        -- This is an incorrect heuristic
        let ready = null (staticDependencies compute k)
        maybeValue <- compute (if ready then getValue else build) k
        case maybeValue of
            Just value -> putValue k value >> return value
            Nothing    -> getValue k

-- | Check that a build system is /idempotent/, i.e. running it once or twice in
-- a row leads to the same 'Plan' and 'Store'.
-- idempotent :: (Eq k, Eq v) => Store m k v => Build m k v -> m Bool
-- idempotent build = forallM $ \(compute, outputs, state, plan) -> do
--     (state', plan' ) <- build compute outputs (state , plan )
--     (_     , plan'') <- build compute outputs (state', plan')
--     return $ forall $ \key -> plan' key == plan'' key
