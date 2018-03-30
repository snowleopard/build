{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes #-}
module Build.Task.MonadPlus (
    random, dependenciesM, transitiveDependencies, acyclic, inputs, isInput,
    correct, pure, computeND
    ) where

import Control.Monad
import Control.Monad.Writer
import Data.Maybe

import Build.Task
import Build.Store
import Build.Utilities

random :: (Int, Int) -> Task MonadPlus k Int
random (low, high) _ _ = Just $ foldr mplus mzero $ map pure [low..high]

dependenciesM :: MonadPlus m => Task MonadPlus k v -> (k -> m v) -> k -> m [k]
dependenciesM task store = execWriterT . sequenceA . task fetch
  where
    fetch k = tell [k] >> lift (store k)

transitiveDependencies :: (Eq k, MonadPlus m) => Task MonadPlus k v
                                          -> (k -> m v) -> k -> m (Maybe [k])
transitiveDependencies task fetch = reachM (dependenciesM task fetch)

acyclic :: (Eq k, MonadPlus m) => Task MonadPlus k v -> (k -> m v) -> k -> m Bool
acyclic task fetch = fmap isJust . transitiveDependencies task fetch

inputs :: (Eq k, MonadPlus m) => Task MonadPlus k v -> (k -> m v) -> k -> m (Maybe [k])
inputs task fetch key = do
    deps <- transitiveDependencies task fetch key
    return $ filter (isInput task) <$> deps

-- | Run a non-deterministic task with a pure lookup function, listing all
-- possible results, including @Nothing@ indicating that a given key is an input.
computeND :: Task MonadPlus k v -> (k -> v) -> k -> Maybe [v]
computeND task store = task (return . store)

correct :: Eq v => Task MonadPlus k v -> Store i k v -> Store i k v -> k -> Bool
correct task store result k = case computeND task (flip getValue store) k of
    Nothing -> getValue k result == getValue k store
    Just vs -> getValue k result `elem` vs

-- | Given a task description @task@, a target @key@, an initial @store@, and a
-- @result@ produced by running a build system with parameters @task@, @key@ and
-- @store@, this function returns 'True' if @result@ is a correct build outcome.
-- Specifically:
-- * @result@ and @store@ must agree on the values of all inputs. In other words,
--   no inputs were corrupted during the build.
-- * @result@ is /consistent/ with the @task@, i.e. for all non-input keys, the
--   result of recomputing the @task@ matches the value stored in the @result@.
-- correctBuild :: (Eq k, Eq v) => Task Monad k v -> Store i k v -> Store i k v -> k -> Bool
-- correctBuild task store result key = all correct (reachable deps key)
--   where
--     deps    k = maybe [] snd (track task (\k -> getValue k store) k)
--     correct k = case compute task (flip getValue store) k of
--         Nothing -> getValue k result == getValue k store
--         Just v  -> getValue k result == v

-- pureInputs :: Eq k => Task MonadPlus k v -> (k -> v) -> k -> [Maybe [k]]
-- pureInputs task f = runIdentity . runListT . inputs task (ListT . return . pure . f)

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

-- instance MonadPlus (Script k v) where
--     empty = Zero
--     (<|>) = Plus

-- getScript :: Task MonadPlus k v -> k -> Script k v (Maybe v)
-- getScript task = task Get
