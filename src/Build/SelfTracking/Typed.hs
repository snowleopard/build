{-# LANGUAGE GADTs, ImpredicativeTypes, ScopedTypeVariables #-}

module Build.SelfTracking.Typed (
    Fetch, TaskT (..), TasksT, Key (..), selfTracking
  ) where

import Build.Task

type Fetch k f = forall v. k v -> f v

newtype TaskT c k v = TaskT { runT :: forall f. c f => Fetch k f -> f v }

type TasksT c k = forall v. k v -> Maybe (TaskT c k v)

-- | The type variable @s@ stands for "scripts" written in some task description
-- language.
data Key k v s a where
    Script :: k -> Key k v s s -- Keys for build scripts
    Value  :: k -> Key k v s v -- Keys for all other values

selfTracking :: forall k v s. (s -> Task Monad k v) -> Tasks Monad k s -> TasksT Monad (Key k v s)
selfTracking parse tasks key = case key of
    Script k -> getScript <$> tasks k
    Value  k -> runScript <$> tasks k
  where
    -- Get the task for building the script
    getScript :: Task Monad k s -> TaskT Monad (Key k v s) s
    getScript task = TaskT $ \fetch -> task (fetch . Script)
    -- Build the script, parse it, and then run the obtained task
    runScript :: Task Monad k s -> TaskT Monad (Key k v s) v
    runScript task = TaskT $ \fetch -> do
        script <- task (fetch . Script)
        parse script (fetch . Value)
