{-# LANGUAGE RankNTypes, TypeApplications #-}

-- | A version of monadic tasks with some support for non-determinism.
module Build.Task.MonadPlus (random, computeND, correctBuildValue) where

import Control.Monad

import Build.Task
import Build.Store

-- | An example of a non-deterministic task: generate a random number from a
-- specified interval.
random :: (Int, Int) -> Task MonadPlus k Int
random (low, high) = Task $ const $ foldr mplus mzero $ map pure [low..high]

-- | Run a non-deterministic task with a pure lookup function, listing all
-- possible results.
computeND :: Task MonadPlus k v -> (k -> v) -> [v]
computeND task store = run task (return . store)

-- | Given a description of @tasks@, an initial @store@, and a @result@ produced
-- by running a build system on a target @key@, this function returns 'True' if
-- the @key@'s value is a possible result of running the associated task.
correctBuildValue :: Eq v => Tasks MonadPlus k v -> Store i k v -> Store i k v -> k -> Bool
correctBuildValue tasks store result k = case tasks k of
    Nothing   -> getValue k result == getValue k store
    Just task -> getValue k result `elem` computeND task (flip getValue store)
