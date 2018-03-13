{-# LANGUAGE RankNTypes #-}
module Development.Build.Task.Applicative (
    pureTask, dependencies, transitiveDependencies, acyclic, debugPartial,
    partial, exceptional
    ) where

import Control.Applicative
import Data.Functor.Compose
import Data.Maybe

import Development.Build.Task
import Development.Build.Utilities

-- | Lift a pure function to an applicative task.
pureTask :: (k -> v) -> Task Applicative k v
pureTask store _ = Just . pure . store

-- TODO: Does this always terminate? It's not obvious!
dependencies :: Task Applicative k v -> k -> [k]
dependencies task = maybe [] getConst . task (\k -> Const [k])

transitiveDependencies :: Eq k => Task Applicative k v -> k -> Maybe [k]
transitiveDependencies task = reach (dependencies task)

acyclic :: Eq k => Task Applicative k v -> k -> Bool
acyclic task = isJust . transitiveDependencies task

-- | Run a task with a partial lookup function. The result @Left k@ indicates
-- that the task failed due to a missing dependency @k@. Otherwise, the
-- result @Right (Just v)@ yields the computed value, and @Right Nothing@ is
-- returned if the given key is an input.
debugPartial :: Applicative f => Task Applicative k v
                            -> (k -> f (Maybe v)) -> k -> Maybe (f (Either k v))
debugPartial task store = fmap getCompose . task (Compose . fetch)
  where
    fetch k = maybe (Left k) Right <$> store k

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a partial lookup function @k -> m (Maybe v)@. This essentially lifts the
-- task from the type of values @v@ to @Maybe v@, where the result @Nothing@
-- indicates that the task failed because of a missing dependency.
-- Use 'debugPartial' if you need to know which dependency was missing.
partial :: Task Applicative k v -> Task Applicative k (Maybe v)
partial task fetch = fmap getCompose . task (Compose . fetch)

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the task from the type of values @v@ to @Either e v@,
-- where the result @Left e@ indicates that the task failed because of a
-- failed dependency lookup, and @Right v@ yeilds the value otherwise.
exceptional :: Task Applicative k v -> Task Applicative k (Either e v)
exceptional task fetch = fmap getCompose . task (Compose . fetch)
