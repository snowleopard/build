{-# LANGUAGE ConstraintKinds, GADTs, RankNTypes, ScopedTypeVariables #-}

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
selfTracking _     _     (Script _) = Nothing -- Scripts are inputs
selfTracking parse tasks (Value  k) = runScript <$> tasks k
  where
    -- Fetch the script, parse it, and then run the obtained task
    runScript :: Task Monad k s -> TaskT Monad (Key k v s) v
    runScript task = TaskT $ \fetch -> do
        script <- run task (fetch . Script)
        run (parse script) (fetch . Value)
