{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module Build.Task.Monad (
    dependencies, track, trackM, unwrap, inputs, correctBuild, compute, partial,
    exceptional
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Functor.Identity
import Data.Maybe

import Build.Store
import Build.Task
import Build.Task.Wrapped
import Build.Utilities

-- TODO: Does this always terminate? It's not obvious!
dependencies :: Monad m => Task Monad k v -> (k -> m v) -> m [k]
dependencies task store = execWriterT $ task fetch
  where
    fetch k = tell [k] >> lift (store k)

track :: (k -> v) -> Task Monad k v -> (v, [k])
track fetch task = runWriter $ task (\k -> writer (fetch k, [k]))

trackM :: forall m k v. Monad m => Task Monad k v -> (k -> m v) -> m (v, [k])
trackM task fetch = runWriterT $ task trackingFetch
  where
    trackingFetch :: k -> WriterT [k] m v
    trackingFetch k = tell [k] >> lift (fetch k)

isInput :: forall k v. Tasks Monad k v -> k -> Bool
isInput tasks key = isNothing (tasks key :: Maybe ((k -> Maybe v) -> Maybe v))

unwrap :: forall k v. Wrapped Monad k v -> Task Monad k v
unwrap wrapped = runGTask (wrapped f)
  where
    f :: k -> GTask Monad k v v
    f k = GTask $ \f -> f k

inputs :: forall i k v. Ord k => Tasks Monad k v -> Store i k v -> k -> [k]
inputs tasks store = filter (isInput tasks) . reachable deps
  where
    deps = maybe [] (\t -> snd $ track (flip getValue store) (unwrap t)) . tasks

-- | Given a task description @task@, a target @key@, an initial @store@, and a
-- @result@ produced by running a build system with parameters @task@, @key@ and
-- @store@, this function returns 'True' if @result@ is a correct build outcome.
-- Specifically:
-- * @result@ and @store@ must agree on the values of all inputs. In other words,
--   no inputs were corrupted during the build.
-- * @result@ is /consistent/ with the @task@, i.e. for all non-input keys, the
--   result of recomputing the @task@ matches the value stored in the @result@.
correctBuild :: (Ord k, Eq v) => Tasks Monad k v -> Store i k v -> Store i k v -> k -> Bool
correctBuild tasks store result = all correct . reachable deps
  where
    deps = maybe [] (\t -> snd $ track (flip getValue result) (unwrap t)) . tasks
    correct k = case tasks k of
        Nothing -> getValue k result == getValue k store
        Just t  -> getValue k result == compute (unwrap t) (flip getValue result)

-- | Run a task with a pure lookup function. Returns @Nothing@ to indicate
-- that a given key is an input.
compute :: Task Monad k v -> (k -> v) -> v
compute task store = runIdentity $ task (Identity . store)

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a partial lookup function @k -> m (Maybe v)@. This essentially lifts the
-- task from the type of values @v@ to @Maybe v@, where the result @Nothing@
-- indicates that the task failed because of a missing dependency.
partial :: Task Monad k v -> Task Monad k (Maybe v)
partial task fetch = runMaybeT $ task (MaybeT . fetch)

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the task from the type of values @v@ to @Either e v@,
-- where the result @Left e@ indicates that the task failed because of a
-- failed dependency lookup, and @Right v@ yeilds the value otherwise.
exceptional :: Task Monad k v -> Task Monad k (Either e v)
exceptional task fetch = runExceptT $ task (ExceptT . fetch)
