{-# LANGUAGE ConstraintKinds, RankNTypes #-}

-- | The Task abstractions.
module Build.Task (Task (..), Tasks, compose, liftTask, liftTasks) where

import Control.Applicative

-- Ideally we would like to write:
--
--    type Tasks c k v = k -> Maybe (Task c k v)
--    type Task  c k v = forall f. c f => (k -> f v) -> f v
--
-- Alas, we can't since it requires impredicative polymorphism and GHC currently
-- does not support it.
--
-- A usual workaround is to wrap 'Task' into a newtype, but this leads to the
-- loss of higher-rank polymorphism: for example, we can no longer apply a
-- monadic build system to an applicative task description or apply a monadic
-- 'trackM' to trace the execution of a 'Task Applicative'. This leads to code
-- duplication in some places.

-- | A 'Task' is used to compute a value of type @v@, by finding the necessary
-- dependencies using the provided @fetch :: k -> f v@ callback.
newtype Task c k v = Task { run :: forall f. c f => (k -> f v) -> f v }

-- | 'Tasks' associates a 'Task' with every non-input key. @Nothing@ indicates
-- that the key is an input.
type Tasks c k v = k -> Maybe (Task c k v)

-- | Compose two task descriptions, preferring the first one in case there are
-- two tasks corresponding to the same key.
compose :: Tasks Monad k v -> Tasks Monad k v -> Tasks Monad k v
compose t1 t2 key = t1 key <|> t2 key

-- | Lift an applicative task to @Task Monad@. Use this function when applying
-- monadic task combinators to applicative tasks.
liftTask :: Task Applicative k v -> Task Monad k v
liftTask (Task task) = Task task

-- | Lift a collection of applicative tasks to @Tasks Monad@. Use this function
-- when building applicative tasks with a monadic build system.
liftTasks :: Tasks Applicative k v -> Tasks Monad k v
liftTasks = fmap (fmap liftTask)
