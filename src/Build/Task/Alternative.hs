module Build.Task.Alternative (failingTask, (|||), random, dependencies) where

import Control.Applicative

import Build.Task
import Build.Utilities

-- | The task that always fails by returning 'empty'.
failingTask :: Task Alternative k v
failingTask = Task $ const empty

-- | Run the first task then the second task, combining the results.
(|||) :: Task Alternative k v -> Task Alternative k v -> Task Alternative k v
(|||) task1 task2 = Task $ \fetch -> run task1 fetch <|> run task2 fetch

random :: (Int, Int) -> Task Alternative k Int
random (low, high) = Task $ const $ foldr (<|>) empty $ map pure [low..high]

dependencies :: Task Alternative k v -> [[k]]
dependencies task = getAltConst $ run task (\k -> AltConst [[k]])
