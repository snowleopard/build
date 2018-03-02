module Development.Build.Compute.Alternative (failingCompute) where

import Control.Applicative

import Development.Build.Compute

-- | The computation that fails for any key by returning 'empty'.
failingCompute :: Alternative f => Compute f k v
failingCompute _ _ = empty
