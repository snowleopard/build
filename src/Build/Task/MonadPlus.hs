{-# LANGUAGE FlexibleContexts #-}
module Build.Task.MonadPlus (
    random, dependenciesM, computeND, correctBuild
    ) where

import Control.Monad
import Control.Monad.Writer

import Build.Task
import Build.Store

random :: (Int, Int) -> Task MonadPlus k Int
random (low, high) = Task $ const $ foldr mplus mzero $ map pure [low..high]

dependenciesM :: MonadPlus m => Task MonadPlus k v -> (k -> m v) -> m [k]
dependenciesM task store = execWriterT $ run task fetch
  where
    fetch k = tell [k] >> lift (store k)

-- | Run a non-deterministic task with a pure lookup function, listing all
-- possible results, including @Nothing@ indicating that a given key is an input.
computeND :: Task MonadPlus k v -> (k -> v) -> [v]
computeND task store = run task (return . store)

correctBuild :: Eq v => Tasks MonadPlus k v -> Store i k v -> Store i k v -> k -> Bool
correctBuild tasks store result k = case tasks k of
    Nothing   -> getValue k result == getValue k store
    Just task -> getValue k result `elem` computeND task (flip getValue store)
