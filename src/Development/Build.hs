{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables #-}
module Development.Build (
    -- * Build
    Build,

    -- * Properties
    consistent, correct, idempotent
    ) where

import Development.Build.Compute hiding (consistent)
import Development.Build.NonDeterministic
import Development.Build.Plan hiding (consistent)
import Development.Build.Store hiding (consistent)
import Development.Build.Utilities

import qualified Development.Build.Compute as C
import qualified Development.Build.Plan    as P

-- | Check a three-way consistency between a 'Compute' function, a 'Plan' and
-- a 'Store' with respect to a given key. This involves checking the following:
-- * The plan is complete, i.e. all dependencies of the key are known.
-- * The ('Plan', 'Store') pair agrees with the 'Compute' function.
consistent :: (Eq k, Eq v) => Compute k v -> Plan k v -> Store k v -> k -> Bool
consistent compute plan store key = case plan key of
    Nothing        -> False -- The plan is incomplete
    Just (_, deps) -> (getValue store key, map fst deps) `member` compute store key
                   && and [ consistent compute plan store k | (k, _) <- deps ]

-- | A list of keys that need to be built.
type Outputs k = [k]

-- | Some build systems maintain a persistent state between builds for the
-- purposes of optimisation and profiling. This can include a cache for sharing
-- build results across builds.
data State k v

-- | A build system takes a 'Compute' and 'Outputs' and returns the transformer
-- of the triple ('State', 'Plan', 'Store').
type Build k v = Compute k v -> Outputs k -> (State k v, Plan k v, Store k v)
                                          -> (State k v, Plan k v, Store k v)

-- | Check that a build system is correct, i.e. for all possible combinations of
-- input parameters ('Compute', 'Outputs', 'State', 'Plan', 'Store'), where
-- 'Compute' is 'wellDefined', the build system produces a correct output pair
-- (@newPlan@, @newStore@). Specifically, there exists a @magicStore@, such that:
-- * The @newPlan@ is acyclic.
-- * The @oldstore@, the @newStore@ and the @magicStore@ agree on the input keys.
-- * The @newStore@ and the @magicStore@ agree on the output keys.
-- * The @magicStore@ is consistent w.r.t. the @compute@ function and the @plan@.
-- There are no correctness requirements on the resulting 'State'.
correct :: (Eq k, Eq v) => Build k v -> Bool
correct build = forall $ \(compute, outputs, state, oldPlan, oldStore) ->
    C.consistent compute && P.consistent oldPlan oldStore ==> exists $ \magicStore ->
        let (_, newPlan, newStore) = build compute outputs (state, oldPlan, oldStore) in
        -- The new plan is acyclic and consistent
        acyclic newPlan && P.consistent newPlan newStore
        &&
        -- The oldStore, newStore and the magicStore agree on the inputs
        all (\k -> getHash oldStore k == getHash newStore k
                && getHash oldStore k == getHash magicStore k)
            (concatMap (inputs newPlan) outputs)
        &&
        -- The newStore and the magicStore agree on the outputs
        all (\k -> getHash newStore k == getHash magicStore k) outputs
        &&
        -- The magicStore is consistent w.r.t. the compute function and the plan
        all (consistent compute newPlan magicStore) outputs

-- | Check that a build system is /idempotent/, i.e. running it once or twice in
-- a row leads to the same 'Plan' and 'Store'.
idempotent :: (Eq k, Eq v) => Build k v -> Bool
idempotent build = forall $ \(compute, keys, state, plan, store) ->
    let (state', plan' , store' ) = build compute keys (state , plan , store )
        (_     , plan'', store'') = build compute keys (state', plan', store')
    in forall $ \key ->          plan'' key == plan' key
                     && getHash store'' key == getHash store' key
