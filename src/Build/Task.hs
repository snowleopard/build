{-# LANGUAGE ImpredicativeTypes #-}

-- | The Task abstractions.
module Build.Task (Task, Tasks, compose) where

import Control.Applicative

-- | A 'Task' is used to compute a value of type @v@, by finding the necessary
-- dependencies using the provided @fetch :: k -> f v@ callback.
type Task c k v = forall f. c f => (k -> f v) -> f v

-- | 'Tasks' associates a 'Task' with every non-input key. @Nothing@ indicates
-- that the key is an input.
type Tasks c k v = k -> Maybe (Task c k v)

-- | Compose two task descriptions, preferring the first one in case there are
-- two tasks corresponding to the same key.
compose :: Tasks Monad k v -> Tasks Monad k v -> Tasks Monad k v
compose t1 t2 key = t1 key <|> t2 key
