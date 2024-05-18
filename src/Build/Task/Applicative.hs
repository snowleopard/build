{-# LANGUAGE ImpredicativeTypes #-}

-- | Applicative tasks, as used by Make, Ninja and other applicative build
-- systems. Dependencies of applicative tasks are known statically, before their
-- execution.
module Build.Task.Applicative (dependencies) where

import Control.Applicative

import Build.Task

-- | Find the dependencies of an applicative task.
dependencies :: Task Applicative k v -> [k]
dependencies task = getConst $ task (\k -> Const [k])
