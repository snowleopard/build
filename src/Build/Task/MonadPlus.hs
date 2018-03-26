{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes #-}
module Build.Task.MonadPlus (
    random, dependencies, transitiveDependencies, acyclic, inputs, isInput,
    consistent, pure
    ) where

import Control.Monad
import Control.Monad.List
import Control.Monad.Writer
import Data.Functor.Identity
import Data.Maybe

import Build.Task
import Build.Utilities

random :: (Int, Int) -> Task MonadPlus k Int
random (low, high) _ _ = Just $ foldr mplus mzero $ map pure [low..high]

-- Can we have something like this instead?
-- dependencies :: c m => Task c k v -> (k -> m v) -> k -> m [k]

dependencies :: MonadPlus m => Task MonadPlus k v -> (k -> m v) -> k -> m [k]
dependencies task store = execWriterT . sequenceA . task fetch
  where
    fetch k = tell [k] >> lift (store k)

transitiveDependencies :: (Eq k, MonadPlus m) => Task MonadPlus k v
                                          -> (k -> m v) -> k -> m (Maybe [k])
transitiveDependencies task fetch = reachM (dependencies task fetch)

acyclic :: (Eq k, MonadPlus m) => Task MonadPlus k v -> (k -> m v) -> k -> m Bool
acyclic task fetch = fmap isJust . transitiveDependencies task fetch

inputs :: (Eq k, MonadPlus m) => Task MonadPlus k v -> (k -> m v) -> k -> m (Maybe [k])
inputs task fetch key = do
    deps <- transitiveDependencies task fetch key
    return $ filter (isInput task) <$> deps

-- | Check that a non-deterministic task is /consistent/ with a pure lookup
-- function @f@, i.e. for all keys @k@, if @f k == v@ then @Just v@ is a possible
-- result of the task.
consistent :: Eq v => Task MonadPlus k v -> (k -> v) -> Bool
consistent task store = forall $ \k -> case computeND task store k of
    Nothing -> True
    Just vs -> store k `elem` vs

-- | Run a non-deterministic task with a pure lookup function, listing all
-- possible results, including @Nothing@ indicating that a given key is an input.
computeND :: Task MonadPlus k v -> (k -> v) -> k -> Maybe [v]
computeND task store = fmap runList . task (list . pure . store)

type List a = ListT Identity a

runList :: List a -> [a]
runList = runIdentity . runListT

list :: [a] -> List a
list = ListT . Identity

-- pureInputs :: Eq k => Task MonadPlus k v -> (k -> v) -> k -> [Maybe [k]]
-- pureInputs task f = runIdentity . runListT . inputs task (ListT . return . pure . f)

-- | Given a @task@, a pair of key-value maps describing the contents of a
-- store @before@ and @after@ a build system was executed to build a given list
-- of @outputs@, determine if @after@ is a correct build outcome.
-- Specifically, there must exist a @magic@ key-value map, such that:
-- * @before@, @after@ and @magic@ agree on the values of all inputs.
-- * @after@ and @magic@ agree on the values of all outputs.
-- * @magic@ is 'consistent' with the @task@.
-- We assume that @task@ is acyclic. If it is not, the function returns @True@.
-- correctBuild :: (Eq k, Eq v) => Task Monad k v -> (k -> v) -> (k -> v) -> [k] -> Bool
-- correctBuild task before after outputs =
--     case concat <$> traverse (pureInputs task after) outputs of
--         Nothing     -> True -- We assumed that task is acyclic, but it is not
--         Just inputs -> exists $ \magic -> agree [before, after, magic] inputs
--                                     && agree [        after, magic] outputs
--                                     && consistent task magic


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
