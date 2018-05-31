{-# LANGUAGE RankNTypes #-}
module Build.Task.Alternative (failingTask, (|||), random, dependencies) where

import Control.Applicative

import Build.Task
import Build.Utilities

-- | The task that always fails by returning 'empty'.
failingTask :: Task Alternative k v
failingTask = const empty

-- | Run the first task then the second task, combining the results.
(|||) :: Task Alternative k v -> Task Alternative k v -> Task Alternative k v
(|||) task1 task2 fetch = task1 fetch <|> task2 fetch

random :: (Int, Int) -> Task Alternative k Int
random (low, high) = const $ foldr (<|>) empty $ map pure [low..high]

dependencies :: Task Alternative k v -> [[k]]
dependencies task = getAltConst $ task (\k -> AltConst [[k]])
