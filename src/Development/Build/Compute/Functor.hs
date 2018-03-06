{-# LANGUAGE RankNTypes #-}
module Development.Build.Compute.Functor (inputCompute, dependency) where

import Data.Functor.Const

import Development.Build.Compute

dependency :: Compute Functor k v -> k -> Maybe k
dependency compute = fmap getConst . compute Const
