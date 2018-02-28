{-# LANGUAGE ConstraintKinds, RankNTypes #-}
module Development.Build (
    -- * Build
    Build, dumbBuild, dumbTracingBuild, State (..), Outputs,

    -- * Properties
    consistent --, correct, idempotent
    ) where

import Control.Monad.IO.Class
import Data.Functor

import Development.Build.Compute
import Development.Build.Compute.Monad
import Development.Build.Plan hiding (consistent)
import Development.Build.Store

-- | Check a three-way consistency between a 'Compute' function, a 'Plan' and
-- a 'Store' with respect to a given key. This involves checking the following:
-- * The plan is complete, i.e. all dependencies of the key are known.
-- * The ('Plan', 'Store') pair agrees with the 'Compute' function.
consistent :: (Eq v, Monad m, GetHash m k v) => Compute (Monad m) m k v -> Plan k v -> k -> m Bool
consistent compute plan key = case plan key of
    Nothing -> return False -- The plan is incomplete
    Just (h, deps) -> do
        h' <- getHash key
        vc <- compute key
        vs <- getValue key
        cs <- mapM (consistent compute plan . fst) deps
        return $ h' == h && vc == vs && and cs

-- | A list of keys that need to be built.
type Outputs k = [k]

-- TODO: Make 'Plan' a part of 'State'.
-- | Some build systems maintain a persistent state between builds for the
-- purposes of optimisation and profiling. This can include a cache for sharing
-- build results across builds.
data State k v = State

-- | A build system takes a 'Compute' and 'Outputs' and returns the transformer
-- of the triple ('State', 'Plan', 'Store').
type Build c f k v = Compute c f k v -> Outputs k
                  -> (State k v, Plan k v) -> f (State k v, Plan k v)
dumbBuild :: (Monad m, Get m k v, Put m k v)
    => (forall n. Compute (Monad n, Get n k v) n k v)
    -> Outputs k
    -> (State k v, Plan k v)
    -> m (State k v, Plan k v)
dumbBuild compute outputs (state, plan) = mapM build outputs $> (state, plan)
  where
    build k = compute k >>= putValue k

dumbTracingBuild :: (MonadIO m, Get m k v, Put m k v, Show k, Show v)
    => (forall n. Compute (Monad n, Get n k v) n k v)
    -> Outputs k
    -> (State k v, Plan k v)
    -> m (State k v, Plan k v)
dumbTracingBuild compute outputs (state, plan) = mapM build outputs $> (state, plan)
  where
    build k = do
        let script       = getScript compute k
            myGetValue k = do
                v <- getValue k
                liftIO $ putStrLn $ "Looked up key: " ++ show k ++ " => " ++ show v
                return v
        liftIO $ putStrLn ("Computing key: " ++ show k)
        v <- runWith myGetValue script
        putValue k v

-- | Check that a build system is correct, i.e. for all possible combinations of
-- input parameters ('Compute', 'Outputs', 'State', 'Plan', 'Store'), where
-- 'Compute' is 'wellDefined', the build system produces a correct output pair
-- (@newPlan@, @newStore@). Specifically, there exists a @magicStore@, such that:
-- * The @newPlan@ is acyclic.
-- * The @oldstore@, the @newStore@ and the @magicStore@ agree on the input keys.
-- * The @newStore@ and the @magicStore@ agree on the output keys.
-- * The @magicStore@ is consistent w.r.t. the @compute@ function and the @plan@.
-- There are no correctness requirements on the resulting 'State'.
-- correct :: (Eq v, Get m k v, GetHash m k v) => Build (Monad m) m k v -> m Bool
-- correct build = undefined
    -- forallM $ \(compute, outputs, state, oldPlan, oldStore) ->
    -- existsM $ \magicStore -> do
    --     (_, newPlan, newStore) <- build compute outputs (state, oldPlan, oldStore)
    --     -- The new plan is acyclic and consistent
    --     let pAcyclic = acyclic newPlan
    --     pConsistent <- P.consistent newPlan newStore
    --     buildInputs <- concat <$> mapM (inputs newPlan) outputs
    --     -- The oldStore, newStore and the magicStore agree on the inputs
    --     agreeInputs <- agree [oldStore, newStore, magicStore] buildInputs
    --     -- The newStore and the magicStore agree on the outputs
    --     agreeOutputs <- agree [newStore, magicStore] outputs
    --     -- The magicStore is consistent w.r.t. the compute function and the plan
    --     sConsistent <- and <$> mapM (consistent compute newPlan magicStore) outputs
    --     return $ pAcyclic && pConsistent && agreeInputs && agreeOutputs

-- | Check that a build system is /idempotent/, i.e. running it once or twice in
-- a row leads to the same 'Plan' and 'Store'.
-- idempotent :: (Eq k, Eq v) => Store m k v => Build m k v -> m Bool
-- idempotent build = forallM $ \(compute, outputs, state, plan) -> do
--     (state', plan' ) <- build compute outputs (state , plan )
--     (_     , plan'') <- build compute outputs (state', plan')
--     return $ forall $ \key -> plan' key == plan'' key
