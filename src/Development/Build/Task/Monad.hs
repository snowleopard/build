{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Task.Monad (
    dependencies, track, trackM, transitiveDependencies, inputs, acyclic,
    consistent, correctBuild, execute, debugPartial, partial, exceptional,
    staticDependencies, Script (..), getScript, runScript, isStatic, execute'
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Functor.Identity
import Data.Maybe
import Data.Proxy

import Development.Build.Store
import Development.Build.Task hiding (isInput)
import Development.Build.Utilities

-- TODO: Does this always terminate? It's not obvious!
dependencies :: Monad m => Task Monad k v -> (k -> m v) -> k -> m [k]
dependencies task store = execWriterT . sequenceA . task fetch
  where
    fetch k = tell [k] >> lift (store k)

track :: Task Monad k v -> Store i k v -> k -> Maybe (v, [k])
track task store = fmap runWriter . task (\k -> writer (getValue store k, [k]))

trackM :: Monad m => Task Monad k v -> (k -> m v) -> k -> Maybe (m (v, [k]))
trackM task store = fmap runWriterT . task fetch
  where
    fetch k = tell [k] >> lift (store k)

transitiveDependencies :: (Eq k, Monad m) => Task Monad k v
                                          -> (k -> m v) -> k -> m (Maybe [k])
transitiveDependencies task fetch = reachM (dependencies task fetch)

acyclic :: (Eq k, Monad m) => Task Monad k v -> (k -> m v) -> k -> m Bool
acyclic task fetch = fmap isJust . transitiveDependencies task fetch

isInput :: Task Monad k v -> k -> Bool
isInput task = isNothing . task (const Proxy)

inputs :: Eq k => Task Monad k v -> Store i k v -> k -> [k]
inputs task store key = filter (isInput task) (closure deps key)
  where
    deps k = maybe [] snd (track task store k)

-- | Check that a task is /consistent/ with a pure lookup function @f@, i.e.
-- if it returns @Just v@ for some key @k@ then @f k == v@.
consistent :: Eq v => Task Monad k v -> Store i k v -> Bool
consistent task store =
    forall $ \k -> case execute task (getValue store) k of
        Nothing -> True
        Just v  -> getValue store k == v

-- | Given a @task@, a pair of key-value maps describing the contents of a
-- store @store@ and @result@ a build system was executed to build a given @key@,
-- determine if @result@ is a correct build outcome.
-- Specifically, there must exist a @ideal@ key-value map, such that:
-- * @store@, @result@ and @ideal@ agree on the values of all inputs.
-- * @result@ and @ideal@ agree on the value of the output @key@.
-- * @ideal@ is 'consistent' with the @task@.
-- We assume that @task@ is acyclic. If it is not, the function returns @True@.
correctBuild :: (Eq k, Eq v) => Task Monad k v -> Store i k v -> Store i k v -> k -> Bool
correctBuild task store result key =
    exists $ \ideal -> agree [store, result, ideal] (inputs task result key)
                    && agree [       result, ideal] [key]
                    && consistent task ideal

-- | Run a task with a pure lookup function. Returns @Nothing@ to indicate
-- that a given key is an input.
execute :: Task Monad k v -> (k -> v) -> k -> Maybe v
execute task fetch = fmap runIdentity . task (pure . fetch)

-- The version used in the paper
execute' :: Task Monad k v -> Store i k v -> k -> Maybe v
execute' task store key = runIdentity <$> task (Identity . getValue store) key

-- | Run a task with a partial lookup function. The result @Left k@ indicates
-- that the task failed due to a missing dependency @k@. Otherwise, the
-- result @Right v@ yields the computed value.
debugPartial :: Monad m => Task Monad k v
                      -> (k -> m (Maybe v)) -> k -> Maybe (m (Either k v))
debugPartial task store = fmap runExceptT . task fetch
  where
    fetch k = maybe (throwE k) return =<< lift (store k)

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a partial lookup function @k -> m (Maybe v)@. This essentially lifts the
-- task from the type of values @v@ to @Maybe v@, where the result @Nothing@
-- indicates that the task failed because of a missing dependency.
-- Use 'debugPartial' if you need to know which dependency was missing.
partial :: Task Monad k v -> Task Monad k (Maybe v)
partial task fetch = fmap runMaybeT . task (MaybeT . fetch)

-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the task from the type of values @v@ to @Either e v@,
-- where the result @Left e@ indicates that the task failed because of a
-- failed dependency lookup, and @Right v@ yeilds the value otherwise.
exceptional :: Task Monad k v -> Task Monad k (Either e v)
exceptional task fetch = fmap runExceptT . task (ExceptT . fetch)

-- TODO: Does this always terminate? It's not obvious!
staticDependencies :: Task Monad k v -> k -> [k]
staticDependencies task key = case getScript task key of
    Nothing     -> []
    Just script -> staticScriptDependencies script

data Script k v a where
    Fetch :: k -> Script k v v
    Pure  :: a -> Script k v a
    Ap    :: Script k v (a -> b) -> Script k v a -> Script k v b
    Bind  :: Script k v a -> (a -> Script k v b) -> Script k v b

instance Functor (Script k v) where
    fmap = Ap . Pure

instance Applicative (Script k v) where
    pure  = Pure
    (<*>) = Ap

instance Monad (Script k v) where
    return = Pure
    (>>)   = (*>)
    (>>=)  = Bind

getScript :: Task Monad k v -> k -> Maybe (Script k v v)
getScript task = task Fetch

runScript :: Monad m => (k -> m v) -> Script k v a -> m a
runScript fetch script = case script of
    Fetch k  -> fetch k
    Pure v   -> pure v
    Ap s1 s2 -> runScript fetch s1 <*> runScript fetch s2
    Bind s f -> runScript fetch s >>= fmap (runScript fetch) f

-- TODO: Fix inifinite loop
staticScriptDependencies :: Script k v a -> [k]
staticScriptDependencies script = case script of
    Fetch k  -> [k]
    Pure _   -> []
    Ap s1 s2 -> staticScriptDependencies s1 ++ staticScriptDependencies s2
    Bind s _ -> staticScriptDependencies s

isStatic :: Script k v a -> Bool
isStatic script = case script of
    Fetch _  -> True
    Pure _   -> True
    Ap s1 s2 -> isStatic s1 && isStatic s2
    Bind _ _ -> False
