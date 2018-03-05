module Development.Build.Compute.Alternative (failingCompute) where

import Control.Applicative

import Development.Build.Compute

-- | The compute that fails for any key by returning 'empty'.
failingCompute :: Compute Alternative k v
failingCompute _ _ = empty
