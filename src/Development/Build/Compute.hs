{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses, RankNTypes, ConstraintKinds #-}
module Development.Build.Compute (
    -- * Compute
    computeCObject,
    ) where

import System.FilePath

import Development.Build.Store

-- | Compute a value corresponding to a given key by performing necessary
-- lookups of the dependencies using the provided lookup function. In the
-- simplest case, @f@ could be just a functor, e.g. see 'computeInput', but more
-- interesting computations involve applicative functors (for static dependencies)
-- or monads (for dynamic dependencies). The result is non-deterministic.
-- type Compute c f k v = c => k -> f v

-- TODO: Separate input keys from others.
-- | The default computation that assumes that all files are inputs, including
-- the files that do not exist. Note: there is only one possible implementation
-- of this function, since it has no other way to produce a value of type @f v@.
computeInput :: Get f k v => k -> f v
computeInput = getValue

-- | Compute an object file from the corresponding C source by running @gcc@.
computeCObject :: (Get m FilePath String, Monad m) => FilePath -> m String
computeCObject key | takeExtension key /= "o" = computeInput key
                   | otherwise = do
    let source   = key -<.> "c"       -- Compute source filename, e.g. f.o -> f.c
        gccKey   = "path/to/gcc"
        includes = undefined source   -- The result of running @gcc -M source@
    need (gccKey : source : includes) -- Records the dependencies
    cmd gccKey source
  where
    cmd = undefined :: k -> k -> m String
