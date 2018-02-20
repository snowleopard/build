{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables #-}
module Development.Build where

import Data.String
import System.FilePath

-- | A 'Key' is a name of a file or, more generally, a variable. We use 'FilePath'
-- for prototyping.
type Key = FilePath

-- | A 'Value' is the contents of a file or, more generally, a variable. We use
-- @newtype Value = Value String@ for prototyping.
newtype Value = Value String deriving (Eq, IsString, Show)

-- | A 'Hash' is used for efficient tracking and sharing of build results. We
-- use @newtype Hash = Hash Value@ for prototyping.
newtype Hash = Hash Value deriving (Eq, Show)

-- | Compute the 'Hash' of a given 'Value'. We typically assume cryptographic
-- hashing, e.g. SHA256. We use @hash v = Hash v@ for prototyping.
hash :: Value -> Hash
hash = Hash

-- | Type for a non-deterministic computation, whose result comes from a set of
-- valid results. The list-based encoding is handy for prototyping, since it's
-- already a 'Monad'.
type NonDeterministic a = [a]

-- | Check if a value is a valid result of a non-deterministic computation.
member :: Eq a => a -> NonDeterministic a -> Bool
member = elem

-- | A key-value mapping. For example, a file system. Note that if a file does
-- not exist, the corresponding 'file not found' value (suitably encoded) is
-- still useful and can be tracked by a build system.
type Store = Key -> Value

-- | A dependency comprises a 'Key' and the 'Hash' of its 'Value'.
type Dependency = (Key, Hash)

-- | A build plan, i.e. a partial map from a 'Key' to the 'Hash' of its 'Value',
-- plus a list of its dependencies. Plans are typically reused from one build to
-- the next to avoid rediscovering build dependencies from scratch. Note that
-- builds do not always go according to plan, and the list of dependencies may
-- have to be recomputed during the build.
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

-- | Check that a given 'Plan' is acyclic with respect to a given 'Key', i.e.
-- that there are no cyclic dependencies in its dependency graph.
acyclic :: Plan -> Key -> Bool
acyclic plan key = case plan key of
    Nothing        -> True
    Just (_, deps) -> and [ key /= k && acyclic plan k | (k, _) <- deps ]

-- | Determine the input files of a given 'Key' according to a given 'Plan'.
inputs :: Plan -> Key -> [Key]
inputs plan key = case plan key of
    Nothing -> [] -- If the plan is incomplete, we return an underapproximation
    Just (_, []  ) -> [key] -- This key has no dependencies, so it is an input
    Just (_, deps) -> concat [ inputs plan k | (k, _) <- deps ]

-- | Given a key-value mapping, compute a 'Value' from a given 'Key', performing
-- necessary lookups of the dependencies. Returns both the resulting 'Value' and
-- a list of dependencies of the 'Key'. The result is non-deterministic.
type Compute = Store -> Key -> NonDeterministic (Value, [Dependency])

-- | The default computation that assumes that all files are inputs, including
-- the files that do not exist.
defaultCompute :: Compute
defaultCompute store key = return (store key, [])

-- | Compute an object file from the corresponding C source by running @gcc@.
gccCompute :: Compute
gccCompute store key | takeExtension key /= "o" = defaultCompute store key
                     | otherwise = do
    let source   = key -<.> "c"     -- Compute source filename, e.g. f.o -> f.c
        includes = undefined source -- The result of running @gcc -M source@
        deps     = gccKey : source : includes
    result <- gcc (store source)
    return (result, zip deps $ map (hash . store) deps)
  where
    gcc :: Value -> NonDeterministic Value
    gcc source = undefined source -- The result of running @gcc source@
    gccKey :: Key
    gccKey = "path/to/gcc"

-- | Check a three-way consistency between a 'Compute' function, a 'Plan' and
-- a 'Store' with respect to a given 'Key'. This involves checking the following:
-- * The plan is acyclic and complete, i.e. all dependencies of the key are known.
-- * The plan is consistent with the store.
-- * The ('Plan', 'Store') pair agrees with the 'Compute' function.
consistent :: Compute -> Plan -> Store -> Key -> Bool
consistent compute plan store key = acyclic plan key && case plan key of
    Nothing        -> False -- The plan is incomplete
    Just (h, deps) -> hash (store key) == h
                   && (store key, deps) `member` compute store key
                   && and [ consistent compute plan store k | (k, _) <- deps ]

-- | A 'Target' is a list of keys that need to be built.
type Target = [Key]

-- | Some build systems maintain a persistent state between builds for the
-- purposes of optimisation and profiling. This can include a cache for sharing
-- build results across builds.
data State

-- | A build system takes a 'Compute' and a 'Target' and returns the transformer
-- of the triple ('State', 'Plan', 'Store').
type Build = Compute -> Target -> (State, Plan, Store) -> (State, Plan, Store)

-- | Check that a build system is correct, i.e. for all possible combinations of
-- input parameters ('Compute', 'Target', 'State', 'Plan', 'Store'), the build
-- system produces a correct output pair (@newPlan@, @newStore@). Specifically,
-- there exists a @magicStore@, such that:
-- * The @store@ and the @magicStore@ agree on the input keys.
-- * The @newStore@ and the @magicStore@ agree on the target keys.
-- * The @magicStore@ is consistent w.r.t. the @compute@ function and the @plan@.
-- There are no correctness requirements on the resulting 'State'.
correct :: Build -> Bool
correct build = forall $ \(compute, keys, state, plan, store) -> exists $ \magicStore ->
    let (_, newPlan, newStore) = build compute keys (state, plan, store) in
    -- The store and the magicStore agree on the inputs
    all (\k -> store k == magicStore k) (concatMap (inputs newPlan) keys)
    &&
    -- The newStore and the magicStore agree on the target keys
    all (\k -> newStore k == magicStore k) keys
    &&
    -- The magicStore is consistent w.r.t. the compute function and the plan
    all (consistent compute newPlan magicStore) keys

-- | Check that a predicate holds for all values of @a@.
forall :: (a -> Bool) -> Bool
forall = undefined

-- | Check that a predicate holds for some value of @a@.
exists :: (a -> Bool) -> Bool
exists = undefined

-- | Logical implication.
(==>) :: Bool -> Bool -> Bool
x ==> y = if x then y else True

infixl 1 ==>
