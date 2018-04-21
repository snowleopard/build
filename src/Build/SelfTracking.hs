{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
-- | This module defines 3 different selfTracking strategies. It's all based around the idea of a
--   String (which can be stored) being able to be parsed into a Task.
--
-- * For Monad it works out beautifully. You just store the rule on the disk, and depend on it.
--
-- * For Applicative1, if you are willing to give up cutoff, you can just store the rules as a Tree
--   in the node itself. That works adequately, but in practice I don't think anyone does it like that.
--
-- * For Applicative2, we generate a fresh Task each time, but have that Task depend on a fake version
--   of the rules. This is a change in the Task, but it's one for which the standard implementations
--   tend to cope with just fine. Most Applicative systems with tracing probably actually do it this way.
module Build.SelfTracking(
    KeyM (..), selfTrackingM,
    KeyA1 (..), selfTrackingA1,
    KeyA2 (..), ValueA2 (..), selfTrackingA2
    ) where

import Data.Tree

import Build.Task
import Build.Task.Depend

data KeyM k     = KeyM k   | KeyTaskM k
data ValueM v t = ValueM v | ValueTaskM t

-- | A model using Monad, works beautifully and allows storing the key on the disk
selfTrackingM :: (t -> Task Monad k v) -> Tasks Monad (KeyM k) (ValueM v t)
selfTrackingM _     (KeyTaskM _) = Nothing -- the Tasks must be an input stored on disk
selfTrackingM parse (KeyM     k) = Just $ Task $ \fetch -> do
    ValueTaskM taskDescription <- fetch (KeyTaskM k)
    ValueM <$> run (parse taskDescription) (fmap (\(ValueM v) -> v) . fetch . KeyM)

data KeyA1 k = KeyA1 k (Tree String)

-- | First Applicative model, requires every key to be able to associate with it's environment (e.g. a reader somewhere)
--   Does not support cutoff if a key changes
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
