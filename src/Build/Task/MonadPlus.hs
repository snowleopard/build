{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module Build.Task.MonadPlus (
    random, dependenciesM, unwrap, computeND, correctBuild
    ) where

import Control.Monad
import Control.Monad.Writer

import Build.Task
import Build.Task.Wrapped
import Build.Store

random :: (Int, Int) -> Task MonadPlus k Int
random (low, high) = const $ foldr mplus mzero $ map pure [low..high]

dependenciesM :: MonadPlus m => Task MonadPlus k v -> (k -> m v) -> m [k]
dependenciesM task store = execWriterT $ task fetch
  where
    fetch k = tell [k] >> lift  (store k)

unwrap :: forall k v. Wrapped MonadPlus k v -> Task MonadPlus k v
unwrap wrapped = runTask (wrapped f)
  where
    f :: k -> ReifiedTask MonadPlus k v v
    f k = ReifiedTask $ \f -> f k

-- | Run a non-deterministic task with a pure lookup function, listing all
-- possible results, including @Nothing@ indicating that a given key is an input.
computeND :: Task MonadPlus k v -> (k -> v) -> [v]
computeND task store = task (return . store)

correctBuild :: Eq v => Tasks MonadPlus k v -> Store i k v -> Store i k v -> k -> Bool
correctBuild tasks store result k = case tasks k of
    Nothing -> getValue k result == getValue k store
    Just t -> getValue k result `elem` computeND (unwrap t) (flip getValue store)
