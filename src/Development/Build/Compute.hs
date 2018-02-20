{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Development.Build.Compute (Compute, wellDefined) where

import Data.String
import System.FilePath

import Development.Build.NonDeterministic
import Development.Build.Store
import Development.Build.Utilities

-- | Compute a 'Value' corresponding to a given 'Key', by performing necessary
-- lookups of the dependencies in a given 'Store'. If there are no dependencies,
-- the 'Key' is an input and the 'Value' is computed by looking it up in the
-- 'Store' (in this case the result is deterministic). Otherwise the 'Key' has
-- one or more dependencies, one of which is a (potentially non-deterministic)
-- 'what to do' function, e.g. @gcc@. Returns both the resulting 'Value' and a
-- list of dependency keys. The result is non-deterministic. A /well-defined/
-- 'Compute' function must be acyclic and report the exact set of dependencies,
-- i.e. a 'Key' must be listed if and only if we looked it up in the 'Store'.
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

-- TODO: Check that the list of dependencies is accurate.
-- | Check that a 'Compute' function never produces cyclic dependencies. Ideally
-- we also want to check that the reported list of dependencies is accurate, but
-- at the moment it is unclear how to achieve that.
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
