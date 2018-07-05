{-# LANGUAGE ScopedTypeVariables #-}

-- | Monadic tasks, as used by Excel, Shake and other build systems.
-- Dependencies of monadic tasks can only be discovered dynamically, i.e. during
-- their execution.
module Build.Task.Monad (
    track, trackM, isInput, computePure, compute, partial, exceptional
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Functor.Identity
import Data.Maybe

import Build.Store
import Build.Task

-- | Execute a monadic task on a pure store @k -> v@, tracking the dependencies.
track :: Task Monad k v -> (k -> v) -> (v, [k])
track task fetch = runWriter $ run task (\k -> writer (fetch k, [k]))

-- | Execute a monadic task using an effectful fetch function @k -> m v@,
-- tracking the dependencies.
trackM :: forall m k v. Monad m => Task Monad k v -> (k -> m v) -> m (v, [(k, v)])
trackM task fetch = runWriterT $ run task trackingFetch
  where
    trackingFetch :: k -> WriterT [(k,v)] m v
    trackingFetch k = do
        v <- lift $ fetch k
        tell [(k,v)]
        return v

-- | Given a description of tasks, check if a key is input.
isInput :: Tasks Monad k v -> k -> Bool
isInput tasks = isNothing . tasks

-- | Run a task with a pure lookup function.
computePure :: Task Monad k v -> (k -> v) -> v
computePure task store = runIdentity $ run task (return . store)

-- | Run a task in a given store.
compute :: Task Monad k v -> Store i k v -> v
compute task store = runIdentity $ run task (\k -> return (getValue k store))

-- | Convert a task with a total lookup function @k -> m v@ into a task with a
-- partial lookup function @k -> m (Maybe v)@. This essentially lifts the task
-- from the type of values @v@ to @Maybe v@, where the result @Nothing@
-- indicates that the task failed because of a missing dependency.
partial :: Task Monad k v -> Task Monad k (Maybe v)
partial task = Task $ \fetch -> runMaybeT $ run task (MaybeT . fetch)

-- | Convert a task with a total lookup function @k -> m v@ into a task with a
-- lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the task from the type of values @v@ to @Either e v@, where
-- the result @Left e@ indicates that the task failed because of a failed
-- dependency lookup, and @Right v@ yeilds the value otherwise.
exceptional :: Task Monad k v -> Task Monad k (Either e v)
exceptional task = Task $ \fetch -> runExceptT $ run task (ExceptT . fetch)
