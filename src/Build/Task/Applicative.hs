{-# LANGUAGE ConstraintKinds, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Build.Task.Applicative (
    pureTask, dependencies, inputs, partial, exceptional
    ) where

import Control.Applicative
import Data.Functor.Compose
import Data.Maybe
import Data.Proxy

import Build.Task
import Build.Task.Wrapped
import Build.Utilities

-- | An applicative task that returns a constant.
pureTask :: v -> Task Applicative k v
pureTask v = const (pure v)

-- TODO: Does this always terminate? It's not obvious!
dependencies :: Task Applicative k v -> [k]
dependencies task = getConst $ task (\k -> Const [k])

isInput :: forall k v. Tasks Applicative k v -> k -> Bool
isInput tasks key = isNothing (tasks @Proxy key)

inputs :: forall k v. Ord k => Tasks Applicative k v -> k -> [k]
inputs tasks = filter (isInput tasks) . reachable deps
  where
    deps :: k -> [k]
    deps = maybe [] (\task -> dependencies (unwrap @Applicative task)) . tasks

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a partial lookup function @k -> m (Maybe v)@. This essentially lifts the
-- task from the type of values @v@ to @Maybe v@, where the result @Nothing@
-- indicates that the task failed because of a missing dependency.
-- Use 'debugPartial' if you need to know which dependency was missing.
partial :: Task Applicative k v -> Task Applicative k (Maybe v)
partial task fetch = getCompose $ task (Compose . fetch)

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the task from the type of values @v@ to @Either e v@,
-- where the result @Left e@ indicates that the task failed because of a
-- failed dependency lookup, and @Right v@ yeilds the value otherwise.
exceptional :: Task Applicative k v -> Task Applicative k (Either e v)
exceptional task fetch = getCompose $ task (Compose . fetch)
