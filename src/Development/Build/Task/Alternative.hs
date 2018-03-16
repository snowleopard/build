{-# LANGUAGE RankNTypes #-}
module Development.Build.Task.Alternative (
    failingTask, (|||), random, dependencies, transitiveDependencies, acyclic
    ) where

import Control.Applicative
import Data.Maybe

import Development.Build.Task
import Development.Build.Utilities

-- | The task that fails for any key by returning 'empty'.
failingTask :: Task Alternative k v
failingTask _ _ = Just empty

-- | Run the first task then the second task, combining the results.
(|||) :: Task Alternative k v -> Task Alternative k v -> Task Alternative k v
(|||) task1 task2 fetch key = task1 fetch key <|> task2 fetch key

random :: (Int, Int) -> Task Alternative k Int
random (low, high) _ _ = Just $ foldr (<|>) empty $ map pure [low..high]

dependencies :: Task Alternative k v -> k -> [[k]]
dependencies task = getAltConst . sequenceA . task (\k -> AltConst [[k]])

transitiveDependencies :: Eq k => Task Alternative k v -> k -> [Maybe [k]]
transitiveDependencies task = reachM (dependencies task)

acyclic :: Eq k => Task Alternative k v -> k -> Bool
acyclic task = all isJust . transitiveDependencies task

-- Probably not needed
-- data Script k v a where
--     Get  :: k -> Script k v v
--     Pure :: a -> Script k v a
--     Ap   :: Script k v (a -> b) -> Script k v a -> Script k v b
--     Zero :: Script k v a
--     Plus :: Script k v a -> Script k v a -> Script k v a

-- instance Functor (Script k v) where
--     fmap = Ap . Pure

-- instance Applicative (Script k v) where
--     pure  = Pure
--     (<*>) = Ap

-- instance Alternative (Script k v) where
--     empty = Zero
--     (<|>) = Plus

-- getScript :: Task Alternative k v -> k -> Script k v (Maybe v)
-- getScript task = task Get
