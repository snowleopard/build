{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Alternative (
    AlternativeCompute, failingCompute
    ) where

import Control.Applicative

import Development.Build.Compute

-- | The computation that fails for any key by returning 'empty'.
failingCompute :: AlternativeCompute k v
failingCompute _ _ = empty
