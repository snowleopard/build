{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, RankNTypes, TypeFamilies #-}
module Development.Build.Compute (
    -- * Compute
    Compute, computeInput, computeCObject,
    ) where

import System.FilePath

import Development.Build.NonDeterministic
import Development.Build.Store

-- | Compute a value corresponding to a given key, by performing necessary
-- lookups of the dependencies in a given monad, which is typically the 'Store'
-- monad. The result is non-deterministic.
type Compute m k v = k -> m (NonDeterministic v)

-- TODO: Separate input keys from others.
-- | The default computation that assumes that all files are inputs, including
-- the files that do not exist.
computeInput :: Store m k v => Compute m k v
computeInput key = deterministic <$> getValue key

-- | Compute an object file from the corresponding C source by running @gcc@.
computeCObject :: Store m FilePath String => Compute m FilePath String
computeCObject key | takeExtension key /= "o" = computeInput key
                   | otherwise = do
    let source   = key -<.> "c"       -- Compute source filename, e.g. f.o -> f.c
        gccKey   = "path/to/gcc"
        includes = undefined source   -- The result of running @gcc -M source@
    need (gccKey : source : includes) -- Records the dependencies
    cmd gccKey source
  where
    cmd = undefined
