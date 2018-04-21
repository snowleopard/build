{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
-- | This module defines 3 different strategies of self-tracking. It's all based
-- around the idea of storing task descriptions that can be parsed into a Task.
--
-- * For Monad it works out beautifully. You just store the rule on the disk,
--   and depend on it.
--
-- * For Applicative, we generate a fresh Task each time, but have that Task
--   depend on a fake version of the rules. This is a change in the Task, but
--   it's one for which the standard implementations tend to cope with just fine.
--   Most Applicative systems with self-tracking probably do it this way.
module Build.SelfTracking(
    KeyM (..), selfTrackingM,
    KeyA (..), ValueA (..), selfTrackingA
    ) where

import Build.Task

-- We assume that the fetch passed to a Task is consistent and returns values
-- matching the keys. It is possible to switch to typed tasks to check this
-- assumption at compile time, e.g. see "Build.Task.Typed".
data KeyM k     = KeyM k   | KeyTaskM k
data ValueM v t = ValueM v | ValueTaskM t

-- Fetch a value
fetchValueM :: Monad m => (KeyM k -> m (ValueM v t)) -> k -> m v
fetchValueM fetch key = do
    value <- fetch (KeyM key)
    case value of ValueM v -> return v
                  _ -> error "Inconsistent fetch"

-- Fetch a task description
fetchValueTaskM :: Monad m => (KeyM k -> m (ValueM v t)) -> k -> m t
fetchValueTaskM fetch key = do
    value <- fetch (KeyTaskM key)
    case value of ValueTaskM t -> return t
                  _ -> error "Inconsistent fetch"

-- For simplicity our task description parsers are total.
type TaskParser c k v t = t -> Task c k v

-- A model using Monad, works beautifully and allows storing the key on the disk
selfTrackingM :: TaskParser Monad k v t -> Tasks Monad (KeyM k) (ValueM v t)
selfTrackingM _      (KeyTaskM _) = Nothing -- Task keys are inputs
selfTrackingM parser (KeyM     k) = Just $ Task $ \fetch -> do
    task <- parser <$> fetchValueTaskM fetch k -- Fetch and parse the task description
    ValueM <$> run task (fetchValueM fetch)

data KeyA k
    = KeyA k
    | InputA k
    | KeyTaskA k

data ValueA v
    = ValueA v
    | ValueTaskA String

-- | Second Applicative model, requires every key to be able to associate with it's environment (e.g. a reader somewhere)
--   Does not support cutoff if a key changes
selfTrackingA :: (String -> Maybe (Task Applicative k v)) -> (k -> String) -> Tasks Applicative (KeyA k) (ValueA v)
selfTrackingA _     _   (KeyTaskA _) = Nothing
selfTrackingA _     _   (InputA   _) = Nothing
selfTrackingA parse ask (KeyA k) = Just $ Task $ \fetch ->
        fetch (KeyTaskA k) <* case parse $ ask k of
            Nothing -> fetch $ InputA k
            Just (Task op) -> fmap ValueA $ op $ fmap (\(ValueA v) -> v) . fetch . KeyA
