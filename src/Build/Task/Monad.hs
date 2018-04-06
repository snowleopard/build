{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Build.Task.Monad (
    dependencies, track, trackM, inputs, correctBuild, compute,
    debugPartial, partial, trackExceptions, exceptional
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Functor.Identity
import Data.Maybe

import Build.Store
import Build.Task
import Build.Utilities

-- TODO: Does this always terminate? It's not obvious!
dependencies :: Monad m => Task Monad k v -> (k -> m v) -> m [k]
dependencies task store = execWriterT $ run task fetch
  where
    fetch k = tell [k] >> lift (store k)

track :: (k -> v) -> Task Monad k v -> (v, [k])
track fetch task = runWriter $ run task (\k -> writer (fetch k, [k]))

trackM :: forall m k v. Monad m => (k -> m v) -> Task Monad k v -> m (v, [k])
trackM fetch task = runWriterT $ run task trackingFetch
  where
    trackingFetch :: k -> WriterT [k] m v
    trackingFetch k = tell [k] >> lift (fetch k)

inputs :: Ord k => Tasks Monad k v -> Store i k v -> k -> [k]
inputs tasks store = filter (isNothing . tasks) . reachable deps
  where
    deps = maybe [] (snd . track (flip getValue store)) . tasks

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
    deps = maybe [] (snd . track (flip getValue result)) . tasks
    correct k = case tasks k of
        Nothing   -> getValue k result == getValue k store
        Just task -> getValue k result == compute task (flip getValue result)

-- | Run a task with a pure lookup function. Returns @Nothing@ to indicate
-- that a given key is an input.
compute :: Task Monad k v -> (k -> v) -> v
compute task store = runIdentity $ run task (Identity . store)

-- | Run a task with a partial lookup function. The result @Left k@ indicates
-- that the task failed due to a missing dependency @k@. Otherwise, the
-- result @Right v@ yields the computed value.
debugPartial :: Monad m => Task Monad k v
                      -> (k -> m (Maybe v)) -> m (Either k v)
debugPartial task store = runExceptT $ run task fetch
  where
    fetch k = maybe (throwE k) return =<< lift (store k)

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a partial lookup function @k -> m (Maybe v)@. This essentially lifts the
-- task from the type of values @v@ to @Maybe v@, where the result @Nothing@
-- indicates that the task failed because of a missing dependency.
-- Use 'debugPartial' if you need to know which dependency was missing.
partial :: Task Monad k v -> Task Monad k (Maybe v)
partial task = Task $ \fetch -> runMaybeT $ run task (MaybeT . fetch)

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the task from the type of values @v@ to @Either e v@,
-- where the result @Left e@ indicates that the task failed because of a
-- failed dependency lookup, and @Right v@ yeilds the value otherwise.
exceptional :: Task Monad k v -> Task Monad k (Either e v)
exceptional task = Task $ \fetch -> runExceptT $ run task (ExceptT . fetch)

-- Yuck!
trackExceptions :: forall m k v. Monad m => Task Monad k v -> (k -> m (Maybe v))
                           -> m (Either k (v, [k]))
trackExceptions task partialFetch = (fmap convert . runWriterT) $ debugPartial task fetch
  where
    fetch :: k -> WriterT [k] m (Maybe v)
    fetch k = do
        mv <- lift (partialFetch k)
        writer (mv, [k])
    convert :: (Either k v, [k]) -> Either k (v, [k])
    convert (Left  k, _ ) = Left k
    convert (Right v, ks) = Right (v, ks)
