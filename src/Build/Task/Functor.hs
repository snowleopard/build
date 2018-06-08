-- | Functorial tasks, which have exactly one statically known dependency.
-- Docker is an example of a functorial build system: Docker containers are
-- organised in layers, where each layer makes changes to the previous one.
module Build.Task.Functor (dependency) where

import Data.Functor.Const

import Build.Task

-- | Find the dependency of a functorial task.
dependency :: Task Functor k v -> k
dependency task = getConst $ run task Const
