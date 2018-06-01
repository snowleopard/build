{-# LANGUAGE RankNTypes #-}

-- | The simplest type of build system, requiring only 'Functor' constraints.
--   Only able to have exactly one dependency.
module Build.Task.Functor (dependency) where

import Data.Functor.Const

import Build.Task

-- | Find the dependency of a functorial task.
dependency :: Task Functor k v -> k
dependency task = getConst $ task Const
