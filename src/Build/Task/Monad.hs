{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}

-- | Monadic tasks, as used by Excel, Shake and other build systems.
-- Dependencies of monadic tasks can only be discovered dynamically, i.e. during
-- their execution.
module Build.Task.Monad (track, trackM, isInput, compute, partial, exceptional) where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Functor.Identity
import Data.Maybe

import Build.Task

-- | Execute a monadic task on a pure store @k -> v@, tracking the dependencies.
track :: Task Monad k v -> (k -> v) -> (v, [k])
track task fetch = runWriter $ task (\k -> writer (fetch k, [k]))

-- | Execute a monadic task using an effectful fetch function @k -> m v@,
-- tracking the dependencies.
trackM :: forall m k v. Monad m => Task Monad k v -> (k -> m v) -> m (v, [k])
trackM task fetch = runWriterT $ task trackingFetch
  where
    trackingFetch :: k -> WriterT [k] m v
    trackingFetch k = tell [k] >> lift (fetch k)

-- | Given a description of tasks, check if a key is input.
isInput :: forall k v. Tasks Monad k v -> k -> Bool
isInput tasks key = isNothing (tasks key :: Maybe ((k -> Maybe v) -> Maybe v))

-- | Run a task with a pure lookup function.
compute :: Task Monad k v -> (k -> v) -> v
compute task store = runIdentity $ task (Identity . store)

-- | Convert a task with a total lookup function @k -> m v@ into a task with a
-- partial lookup function @k -> m (Maybe v)@. This essentially lifts the task
-- from the type of values @v@ to @Maybe v@, where the result @Nothing@
-- indicates that the task failed because of a missing dependency.
partial :: Task Monad k v -> Task Monad k (Maybe v)
partial task fetch = runMaybeT $ task (MaybeT . fetch)

-- | Convert a task with a total lookup function @k -> m v@ into a task with a
-- lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the task from the type of values @v@ to @Either e v@, where
-- the result @Left e@ indicates that the task failed because of a failed
-- dependency lookup, and @Right v@ yeilds the value otherwise.
exceptional :: Task Monad k v -> Task Monad k (Either e v)
exceptional task fetch = runExceptT $ task (ExceptT . fetch)
