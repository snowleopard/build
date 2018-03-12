{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Monad (
    dependencies, trackDependencies, trackDependenciesM, transitiveDependencies,
    inputs, acyclic, consistent, correctBuild, execute, debugPartial, partial,
    exceptional, staticDependencies, Script (..), getScript, runScript, isStatic
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Functor.Identity
import Data.Maybe

import Development.Build.Compute
import Development.Build.Utilities

-- TODO: Does this always terminate? It's not obvious!
dependencies :: Monad m => Compute Monad k v -> (k -> m v) -> k -> m [k]
dependencies compute get = execWriterT . sequenceA . compute tracingGet
  where
    tracingGet k = tell [k] >> lift (get k)

trackDependencies :: Compute Monad k v -> (k -> v) -> k -> Maybe (v, [k])
trackDependencies compute store = fmap runWriter . compute fetch
  where
    fetch k = tell [k] >> lift (pure (store k))

trackDependenciesM :: Monad m => Compute Monad k v -> (k -> m v) -> k -> Maybe (m (v, [k]))
trackDependenciesM compute store = fmap runWriterT . compute fetch
  where
    fetch k = tell [k] >> lift (store k)

transitiveDependencies :: (Eq k, Monad m) => Compute Monad k v
                                          -> (k -> m v) -> k -> m (Maybe [k])
transitiveDependencies compute get = reachM (dependencies compute get)

acyclic :: (Eq k, Monad m) => Compute Monad k v -> (k -> m v) -> k -> m Bool
acyclic compute get = fmap isJust . transitiveDependencies compute get

inputs :: (Eq k, Monad m) => Compute Monad k v -> (k -> m v) -> k -> m (Maybe [k])
inputs compute get key = do
    deps <- transitiveDependencies compute get key
    return $ filter (isInput compute) <$> deps

pureInputs :: Eq k => Compute Monad k v -> (k -> v) -> k -> Maybe [k]
pureInputs compute f = runIdentity . inputs compute (Identity . f)

-- | Check that a compute is /consistent/ with a pure lookup function @f@, i.e.
-- if it returns @Just v@ for some key @k@ then @f k == v@.
consistent :: Eq v => Compute Monad k v -> (k -> v) -> Bool
consistent compute f = forall $ \k -> maybe True (f k ==) $ execute compute f k

-- | Given a @compute@, a pair of key-value maps describing the contents of a
-- store @before@ and @after@ a build system was executed to build a given @key@,
-- determine if @after@ is a correct build outcome.
-- Specifically, there must exist a @magic@ key-value map, such that:
-- * @before@, @after@ and @magic@ agree on the values of all inputs.
-- * @after@ and @magic@ agree on the value of the output @key@.
-- * @magic@ is 'consistent' with the @compute@.
-- We assume that @compute@ is acyclic. If it is not, the function returns @True@.
correctBuild :: (Eq k, Eq v) => Compute Monad k v -> (k -> v) -> (k -> v) -> k -> Bool
correctBuild compute before after key =
    case pureInputs compute after key of
        Nothing     -> True -- We assumed that compute is acyclic, but it is not
        Just inputs -> exists $ \magic -> agree [before, after, magic] inputs
                                       && agree [        after, magic] [key]
                                       && consistent compute magic

-- | Run a compute with a pure lookup function. Returns @Nothing@ to indicate
-- that a given key is an input.
execute :: Compute Monad k v -> (k -> v) -> k -> Maybe v
execute compute f = fmap runIdentity . compute (Identity . f)

-- | Run a compute with a partial lookup function. The result @Left k@ indicates
-- that the compute failed due to a missing dependency @k@. Otherwise, the
-- result @Right v@ yields the computed value.
debugPartial :: Monad m => Compute Monad k v
                      -> (k -> m (Maybe v)) -> k -> Maybe (m (Either k v))
debugPartial compute partialGet = fmap runExceptT . compute get
  where
    get k = maybe (throwE k) return =<< lift (partialGet k)

-- | Convert a compute with a total lookup function @k -> m v@ into a compute
-- with a partial lookup function @k -> m (Maybe v)@. This essentially lifts the
-- compute from the type of values @v@ to @Maybe v@, where the result @Nothing@
-- indicates that the compute failed because of a missing dependency.
-- Use 'debugPartial' if you need to know which dependency was missing.
partial :: Compute Monad k v -> Compute Monad k (Maybe v)
partial compute get = fmap runMaybeT . compute (MaybeT . get)

-- | Convert a compute with a total lookup function @k -> m v@ into a compute
-- with a lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the compute from the type of values @v@ to @Either e v@,
-- where the result @Left e@ indicates that the compute failed because of a
-- failed dependency lookup, and @Right v@ yeilds the value otherwise.
exceptional :: Compute Monad k v -> Compute Monad k (Either e v)
exceptional compute get = fmap runExceptT . compute (ExceptT . get)

-- TODO: Does this always terminate? It's not obvious!
staticDependencies :: Compute Monad k v -> k -> [k]
staticDependencies compute key = case getScript compute key of
    Nothing     -> []
    Just script -> staticScriptDependencies script

data Script k v a where
    Get  :: k -> Script k v v
    Pure :: a -> Script k v a
    Ap   :: Script k v (a -> b) -> Script k v a -> Script k v b
    Bind :: Script k v a -> (a -> Script k v b) -> Script k v b

instance Functor (Script k v) where
    fmap = Ap . Pure

instance Applicative (Script k v) where
    pure  = Pure
    (<*>) = Ap

instance Monad (Script k v) where
    return = Pure
    (>>)   = (*>)
    (>>=)  = Bind

getScript :: Compute Monad k v -> k -> Maybe (Script k v v)
getScript compute = compute Get

runScript :: Monad m => (k -> m v) -> Script k v a -> m a
runScript get script = case script of
    Get k    -> get k
    Pure v   -> pure v
    Ap s1 s2 -> runScript get s1 <*> runScript get s2
    Bind s f -> runScript get s >>= fmap (runScript get) f

-- TODO: Fix inifinite loop
staticScriptDependencies :: Script k v a -> [k]
staticScriptDependencies script = case script of
    Get k    -> [k]
    Pure _   -> []
    Ap s1 s2 -> staticScriptDependencies s1 ++ staticScriptDependencies s2
    Bind s _ -> staticScriptDependencies s

isStatic :: Script k v a -> Bool
isStatic script = case script of
    Get _    -> True
    Pure _   -> True
    Ap s1 s2 -> isStatic s1 && isStatic s2
    Bind _ _ -> False
