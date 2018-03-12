{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes #-}
module Development.Build.Compute.MonadPlus (
    dependencies, transitiveDependencies, acyclic, inputs, isInput, consistent,
    pure
    ) where

import Control.Monad
import Control.Monad.List
import Control.Monad.Writer
import Data.Functor.Identity
import Data.Maybe

import Development.Build.Compute
import Development.Build.Utilities

-- Can we have something like this instead?
-- dependencies :: c m => Compute c k v -> (k -> m v) -> k -> m [k]

dependencies :: MonadPlus m => Compute MonadPlus k v -> (k -> m v) -> k -> m [k]
dependencies compute store = execWriterT . sequenceA . compute fetch
  where
    fetch k = tell [k] >> lift (store k)

transitiveDependencies :: (Eq k, MonadPlus m) => Compute MonadPlus k v
                                          -> (k -> m v) -> k -> m (Maybe [k])
transitiveDependencies compute fetch = reachM (dependencies compute fetch)

acyclic :: (Eq k, MonadPlus m) => Compute MonadPlus k v -> (k -> m v) -> k -> m Bool
acyclic compute fetch = fmap isJust . transitiveDependencies compute fetch

inputs :: (Eq k, MonadPlus m) => Compute MonadPlus k v -> (k -> m v) -> k -> m (Maybe [k])
inputs compute fetch key = do
    deps <- transitiveDependencies compute fetch key
    return $ filter (isInput compute) <$> deps

-- | Check that a non-deterministic compute is /consistent/ with a pure lookup
-- function @f@, i.e. for all keys @k@, if @f k == v@ then @Just v@ is a possible
-- result of the compute.
consistent :: Eq v => Compute MonadPlus k v -> (k -> v) -> Bool
consistent compute store = forall $ \k -> Just (store k) `elem` runPure compute store k

-- | Run a non-deterministic compute with a pure lookup function, listing all
-- possible results, including @Nothing@ indicating that a given key is an input.
runPure :: Compute MonadPlus k v -> (k -> v) -> k -> [Maybe v]
runPure compute store = runIdentity . runListT . sequenceA . compute (ListT . Identity . pure . store)

-- pureInputs :: Eq k => Compute MonadPlus k v -> (k -> v) -> k -> [Maybe [k]]
-- pureInputs compute f = runIdentity . runListT . inputs compute (ListT . return . pure . f)

-- | Given a @compute@, a pair of key-value maps describing the contents of a
-- store @before@ and @after@ a build system was executed to build a given list
-- of @outputs@, determine if @after@ is a correct build outcome.
-- Specifically, there must exist a @magic@ key-value map, such that:
-- * @before@, @after@ and @magic@ agree on the values of all inputs.
-- * @after@ and @magic@ agree on the values of all outputs.
-- * @magic@ is 'consistent' with the @compute@.
-- We assume that @compute@ is acyclic. If it is not, the function returns @True@.
-- correctBuild :: (Eq k, Eq v) => Compute Monad k v -> (k -> v) -> (k -> v) -> [k] -> Bool
-- correctBuild compute before after outputs =
--     case concat <$> traverse (pureInputs compute after) outputs of
--         Nothing     -> True -- We assumed that compute is acyclic, but it is not
--         Just inputs -> exists $ \magic -> agree [before, after, magic] inputs
--                                     && agree [        after, magic] outputs
--                                     && consistent compute magic


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

-- getScript :: Compute MonadPlus k v -> k -> Script k v (Maybe v)
-- getScript compute = compute Get
