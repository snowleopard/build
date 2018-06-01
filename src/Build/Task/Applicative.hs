{-# LANGUAGE RankNTypes #-}

-- | Applicative dependencies, as provided by @make@, @ninja@ etc.
--   Dependencies are known before any execution begins.
module Build.Task.Applicative (dependencies) where

import Control.Applicative

import Build.Task

-- | Find the dependencies of an applicative task.
dependencies :: Task Applicative k v -> [k]
dependencies task = getConst $ task (\k -> Const [k])
