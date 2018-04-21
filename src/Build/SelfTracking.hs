{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
-- | This module defines 3 different strategies of self-tracking. It's all based
-- around the idea of storing task descriptions that can be parsed into a Task.
--
-- * For Monad it works out beautifully. You just store the rule on the disk,
--   and depend on it.
--
-- * For Applicative1, if you are willing to give up cutoff, you can just store
--   the rules as a Tree in the node itself. That works adequately, but in
--   practice I (Neil) don't think anyone does it like that.
--
-- * For Applicative2, we generate a fresh Task each time, but have that Task
--   depend on a fake version of the rules. This is a change in the Task, but
--   it's one for which the standard implementations tend to cope with just fine.
--   Most Applicative systems with self-tracking probably do it this way.
module Build.SelfTracking(
    KeyM (..), selfTrackingM,
    KeyA1 (..), selfTrackingA1,
    KeyA2 (..), ValueA2 (..), selfTrackingA2
    ) where

import Data.Tree

import Build.Task
import Build.Task.Depend

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

data KeyA1 k = KeyA1 k (Tree String)

-- | First Applicative model, requires every key to be able to associate with
-- its environment (e.g. a reader somewhere). Does not support cutoff if a key changes
selfTrackingA1 :: (String -> Maybe (Task Applicative k v)) -> Tasks Applicative (KeyA1 k) v
selfTrackingA1 parse (KeyA1 _ (Node task children)) = case parse task of
    Nothing -> Nothing
    Just op -> Just $ Task $ \fetch ->
        let Depend ds f = toDepend op
        in fmap f $ traverse fetch $ zipWith KeyA1 ds children


data KeyA2 k
    = KeyA2 k
    | InputA2 k
    | KeyTaskA2 k

data ValueA2 v
    = ValueA2 v
    | ValueTaskA2 String

-- | Second Applicative model, requires every key to be able to associate with it's environment (e.g. a reader somewhere)
--   Does not support cutoff if a key changes
selfTrackingA2 :: (String -> Maybe (Task Applicative k v)) -> (k -> String) -> Tasks Applicative (KeyA2 k) (ValueA2 v)
selfTrackingA2 _     _   (KeyTaskA2 _) = Nothing
selfTrackingA2 _     _   (InputA2   _) = Nothing
selfTrackingA2 parse ask (KeyA2 k) = Just $ Task $ \fetch ->
        fetch (KeyTaskA2 k) <* case parse $ ask k of
            Nothing -> fetch $ InputA2 k
            Just (Task op) -> fmap ValueA2 $ op $ fmap (\(ValueA2 v) -> v) . fetch . KeyA2
