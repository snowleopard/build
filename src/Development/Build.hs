{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables #-}
module Development.Build where

import Data.String
import System.FilePath

import Development.Build.NonDeterministic
import Development.Build.Store
import Development.Build.Utilities

-- | A dependency comprises a 'Key' and the 'Hash' of its 'Value'.
type Dependency = (Key, Hash)

-- | A build plan, i.e. a partial map from a 'Key' to the 'Hash' of its 'Value',
-- plus a list of its dependencies. Plans are typically reused from one build to
-- the next to avoid rediscovering build dependencies from scratch. Note that
-- builds do not always go according to plan, and the list of dependencies may
-- need to be recomputed during the build. A key is considered to be an /input/
-- if the list of its dependencies is empty, i.e. @plan key == Just (h, [])@.
-- If @plan key == Nothing@, we don't know anything about the key and if it is
-- required for building any of the /outputs/ the build system will need to
-- discover its dependencies. We require that every plan is 'acyclic'.
type Plan = Key -> Maybe (Hash, [Dependency])

-- | Example build plan containing information only for a single file:
-- @"f.o" -> Just (hash "1", [("f.c", hash "2"), ("gcc.exe", hash "3")])@.
examplePlan :: Plan
examplePlan key = case key of
    "f.o" -> Just (hash "1", [("f.c", hash "2"), ("gcc.exe", hash "3")])
    _     -> Nothing

-- | Sometimes you have no plan at all, i.e. @emptyPlan = const Nothing@.
emptyPlan :: Plan
emptyPlan = const Nothing

-- | Check that a given 'Plan' has no cyclic dependencies.
acyclic :: Plan -> Bool
acyclic plan = forall $ \key -> key `notElem` dependencies key
  where
    dependencies k = case plan k of
        Nothing        -> []
        Just (_, deps) -> concatMap (dependencies . fst) deps

-- | Find the inputs of a 'Key' that are listed in a given 'Plan'. Note that
-- since the plan can be incomplete, the result may be a subset of the actual
-- set of inputs.
inputs :: Plan -> Key -> [Key]
inputs plan key = case plan key of
    Nothing -> [] -- If the plan is incomplete, we return an underapproximation
    Just (_, []  ) -> [key] -- This key has no dependencies, so it is an input
    Just (_, deps) -> concat [ inputs plan k | (k, _) <- deps ]

-- | Compute a 'Value' corresponding to a given 'Key', by performing necessary
-- lookups of the dependencies in a given 'Store'. If there are no dependencies,
-- the 'Key' is an input and the 'Value' is computed by looking it up in the
-- 'Store' (in this case the result is deterministic). Otherwise the 'Key' has
-- one or more dependencies, one of which is a (potentially non-deterministic)
-- 'what to do' function, e.g. @gcc@. Returns both the resulting 'Value' and a
-- list of dependency keys. The result is non-deterministic.
type Compute = Store -> Key -> NonDeterministic (Value, [Key])

-- | The default computation that assumes that all files are inputs, including
-- the files that do not exist.
defaultCompute :: Compute
defaultCompute store key = return (getValue store key, [])

-- | Compute an object file from the corresponding C source by running @gcc@.
gccCompute :: Compute
gccCompute store key | takeExtension key /= "o" = defaultCompute store key
                     | otherwise = do
    let source   = key -<.> "c"     -- Compute source filename, e.g. f.o -> f.c
        includes = undefined source -- The result of running @gcc -M source@
        depKeys  = gccKey : source : includes
    result <- gcc (getValue store source)
    return (result, depKeys)
  where
    gcc :: Value -> NonDeterministic Value
    gcc source = undefined source -- The result of running @gcc source@
    gccKey :: Key
    gccKey = "path/to/gcc"

-- | Check that a 'Compute' function never produces cyclic dependencies.
wellDefined :: Compute -> Bool
wellDefined compute = forall $ \store -> forall $ \key ->
    let allDeps :: NonDeterministic [Key]
        allDeps = dependencies store key in
        forall (\deps -> deps `member` allDeps ==> key `notElem` deps)
  where
    -- Compute transitive dependencies of a Key
    dependencies :: Store -> Key -> NonDeterministic [Key]
    dependencies s k = do
        (_, deps) <- compute s k
        concat <$> mapM (dependencies s) deps

-- | Check a three-way consistency between a 'Compute' function, a 'Plan' and
-- a 'Store' with respect to a given 'Key'. This involves checking the following:
-- * The plan is acyclic and complete, i.e. all dependencies of the key are known.
-- * The plan is consistent with the store.
-- * The ('Plan', 'Store') pair agrees with the 'Compute' function.
consistent :: Compute -> Plan -> Store -> Key -> Bool
consistent compute plan store key = acyclic plan && case plan key of
    Nothing        -> False -- The plan is incomplete
    Just (h, deps) -> getHash store key == h
                   && (getValue store key, map fst deps) `member` compute store key
                   && and [ consistent compute plan store k | (k, _) <- deps ]

-- | A list of keys that need to be built.
type Outputs = [Key]

-- | Some build systems maintain a persistent state between builds for the
-- purposes of optimisation and profiling. This can include a cache for sharing
-- build results across builds.
data State

-- | A build system takes a 'Compute' and 'Outputs' and returns the transformer
-- of the triple ('State', 'Plan', 'Store').
type Build = Compute -> Outputs -> (State, Plan, Store) -> (State, Plan, Store)

-- | Check that a build system is correct, i.e. for all possible combinations of
-- input parameters ('Compute', 'Outputs', 'State', 'Plan', 'Store'), where
-- 'Compute' is 'wellDefined', the build system produces a correct output pair
-- (@newPlan@, @newStore@). Specifically, there exists a @magicStore@, such that:
-- * The @oldstore@, the @newStore@ and the @magicStore@ agree on the input keys.
-- * The @newStore@ and the @magicStore@ agree on the output keys.
-- * The @magicStore@ is consistent w.r.t. the @compute@ function and the @plan@.
-- There are no correctness requirements on the resulting 'State'.
-- TODO: We also assume the input plan is consistent.
correct :: Build -> Bool
correct build = forall $ \(compute, outputs, state, oldPlan, oldStore) ->
    wellDefined compute ==> exists $ \magicStore ->
        let (_, newPlan, newStore) = build compute outputs (state, oldPlan, oldStore) in
        -- The oldStore, newStore and the magicStore agree on the inputs
        all (\k -> getHash oldStore k == getHash newStore k
                && getHash oldStore k == getHash magicStore k)
            (concatMap (inputs newPlan) outputs)
        &&
        -- The newStore and the magicStore agree on the outputs
        all (\k -> getHash newStore k == getHash magicStore k) outputs
        &&
        -- The magicStore is consistent w.r.t. the compute function and the plan
        -- TODO: Check that the plan is acyclic here
        all (consistent compute newPlan magicStore) outputs

-- | Check that a build system is /idempotent/, i.e. running it once or twice in
-- a row leads to the same 'Plan' and 'Store'.
idempotent :: Build -> Bool
idempotent build = forall $ \(compute, keys, state, plan, store) ->
    let (state', plan' , store' ) = build compute keys (state , plan , store )
        (_     , plan'', store'') = build compute keys (state', plan', store')
    in forall $ \key ->          plan'' key == plan' key
                     && getHash store'' key == getHash store' key

zeroBuild :: Build -> Bool
zeroBuild build = forall $ \(compute, keys, state, plan, store) ->
    let (state', plan' , store' ) = build compute    keys (state , plan , store )
        (_     , plan'', store'') = build badCompute keys (state', plan', store')
    in forall $ \key ->          plan'' key == plan' key
                     && getHash store'' key == getHash store' key

badCompute :: Compute
badCompute store key = undefined
