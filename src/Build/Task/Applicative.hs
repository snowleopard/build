{-# LANGUAGE ConstraintKinds, DeriveFunctor, RankNTypes, ScopedTypeVariables #-}
module Build.Task.Applicative (
    pureTask, dependencies, inputs, partial, exceptional
    ) where

import Control.Applicative
import Data.Functor.Compose
import Data.Maybe
import Data.Proxy

import Build.Task
import Build.Utilities

-- | An applicative task that returns a constant.
pureTask :: v -> Task Applicative k v
pureTask v = Task $ const (pure v)

type Ts c k v = k -> Maybe (TT c k v)
newtype TT c k v = TT { t :: T c k v }
type T c k v = forall f. c f => (k -> f v) -> f v

tf :: T Functor Int Int
tf fetch = (+1) <$> fetch 0

ta :: T Applicative Int Int
ta = tf

ttf :: TT Functor Int Int
ttf = TT tf

tta :: TT Applicative Int Int
tta = TT ta

ds :: T Applicative k v -> [k]
ds task = getConst $ task (\k -> Const [k])

isi :: forall k v. Ts Applicative k v -> k -> Bool
isi tasks = isNothing . tasks

-- wT :: Tsc c k v -> a -> (Tc c k v -> a) -> k -> a
-- wT tasks o i key = case tasks key of
--     Nothing -> o
--     Just t  -> i t

is :: forall k v. Ord k => Ts Applicative k v -> k -> [k]
is tasks = filter (isi tasks) . reachable (maybe [] d . tasks)
  where
    d :: TT Applicative k v -> [k]
    d tt = ds (t tt)


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
