{-# LANGUAGE DeriveFunctor #-}
module Build.Task.Applicative (
    pureTask, dependencies, inputs, partial, exceptional, toDepend, Depend(..)
    ) where

import Control.Applicative
import Data.Functor.Compose
import Data.Maybe

import Build.Task
import Build.Utilities

-- | An applicative task that returns a constant.
pureTask :: v -> Task Applicative k v
pureTask v = Task $ const (pure v)

-- TODO: Does this always terminate? It's not obvious!
dependencies :: Task Applicative k v -> [k]
dependencies task = getConst $ run task (\k -> Const [k])

inputs :: Ord k => Tasks Applicative k v -> k -> [k]
inputs tasks = filter (isNothing . tasks) . reachable (maybe [] dependencies . tasks)

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a partial lookup function @k -> m (Maybe v)@. This essentially lifts the
-- task from the type of values @v@ to @Maybe v@, where the result @Nothing@
-- indicates that the task failed because of a missing dependency.
-- Use 'debugPartial' if you need to know which dependency was missing.
partial :: Task Applicative k v -> Task Applicative k (Maybe v)
partial task = Task $ \fetch -> getCompose $ run task (Compose . fetch)

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the task from the type of values @v@ to @Either e v@,
-- where the result @Left e@ indicates that the task failed because of a
-- failed dependency lookup, and @Right v@ yeilds the value otherwise.
exceptional :: Task Applicative k v -> Task Applicative k (Either e v)
exceptional task = Task $ \fetch -> getCompose $ run task (Compose . fetch)


data Depend k v r = Depend [k] ([v] -> r)
    deriving Functor

instance Applicative (Depend k v) where
    pure v = Depend [] (\[] -> v)
    Depend d1 f1 <*> Depend d2 f2 = Depend (d1++d2) $ \vs -> let (v1,v2) = splitAt (length d1) vs in f1 v1 $ f2 v2

toDepend :: Task Applicative k v -> Depend k v v
toDepend (Task f) = f $ \k -> Depend [k] $ \[v] -> v
