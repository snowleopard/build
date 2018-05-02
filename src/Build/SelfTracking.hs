{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | This module defines 3 different strategies of self-tracking. It's all based
-- around the idea of storing task descriptions that can be parsed into a Task.
--
-- * For Monad it works out beautifully. You just store the rule on the disk,
--   and depend on it.
--
-- * For Applicative, we generate a fresh Task each time, but have that Task
--   depend on a fake version of the rules. This is a change in the Task, but
--   it's one for which the standard implementations tend to cope with just fine.
--   Most Applicative systems with self-tracking probably do it this way.
module Build.SelfTracking (
    Key (..), Value (..), selfTrackingM, selfTrackingA
    ) where

import Build.Task

-- We assume that the fetch passed to a Task is consistent and returns values
-- matching the keys. It is possible to switch to typed tasks to check this
-- assumption at compile time, e.g. see "Build.Task.Typed".
data Key k     = Key k   | KeyTask k
data Value v t = Value v | ValueTask t

-- Fetch a value
fetchValue :: Functor f => (Key k -> f (Value v t)) -> k -> f v
fetchValue fetch key = extract <$> fetch (Key key)
  where
    extract (Value v) = v
    extract _ = error "Inconsistent fetch"

-- Fetch a task description
fetchValueTask :: Functor f => (Key k -> f (Value v t)) -> k -> f t
fetchValueTask fetch key = extract <$> fetch (KeyTask key)
  where
    extract (ValueTask t) = t
    extract _ = error "Inconsistent fetch"

-- A model using Monad, works beautifully and allows storing the key on the disk
selfTrackingM :: (t -> Task Monad k v) -> Tasks Monad k t -> Tasks Monad (Key k) (Value v t)
selfTrackingM _      _     (KeyTask _) = Nothing -- Task keys are inputs
selfTrackingM parser tasks (Key     k) = runTask <$> tasks k
  where
    -- Fetch the task description, parse it, and then run the obtained task
    runTask act fetch = do
        task <- parser <$> act (fetchValueTask fetch)
        Value <$> task (fetchValue fetch)

-- | The Applicative model requires every key to be able to associate with its
-- environment (e.g. a reader somewhere). Does not support cutoff if a key changes
selfTrackingA :: (t -> Task Applicative k v) -> (k -> t) -> Tasks Applicative (Key k) (Value v t)
selfTrackingA _      _   (KeyTask _) = Nothing -- Task keys are inputs
selfTrackingA parser ask (Key k) = Just $ \fetch ->
    fetch (KeyTask k) *> (Value <$> parser (ask k) (fetchValue fetch))
