{-# LANGUAGE ImpredicativeTypes, CPP #-}
-- | Functorial tasks, which have exactly one statically known dependency.
-- Docker is an example of a functorial build system: Docker containers are
-- organised in layers, where each layer makes changes to the previous one.
module Build.Task.Functor (dependency) where

#if __GLASGOW_HASKELL__ < 800
import Control.Applicative
#else
import Data.Functor.Const
#endif

import Build.Task

-- | Find the dependency of a functorial task.
dependency :: Task Functor k v -> k
dependency task = getConst (task Const)
