{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Monad (
    dependencies, transitiveDependencies, acyclic,
    staticDependencies, Script (..), getScript, runScript, isStatic, isInput
    ) where

import Control.Monad.Writer
import Data.Maybe

import Development.Build.Compute
import Development.Build.Store
import Development.Build.Utilities

-- TODO: Does this always terminate? It's not obvious!
dependencies :: Monad m => Compute Monad k v -> (k -> m v) -> k -> m [k]
dependencies compute get = execWriterT . compute tracingGet
  where
    tracingGet k = tell [k] >> lift (get k)

transitiveDependencies :: (Eq k, Monad m) => Compute Monad k v -> (k -> m v) -> k -> m (Maybe [k])
transitiveDependencies compute get = reachM (dependencies compute get)

acyclic :: (Eq k, Monad m) => Compute Monad k v -> (k -> m v) -> k -> m Bool
acyclic compute get = fmap isJust . transitiveDependencies compute get

-- TODO: Does this always terminate? It's not obvious!
staticDependencies :: Compute Monad k v -> k -> [k]
staticDependencies compute = staticScriptDependencies . getScript compute

data Script k v a where
    GetValue :: k -> Script k v v
    Pure     :: a -> Script k v a
    Ap       :: Script k v (a -> b) -> Script k v a -> Script k v b
    Bind     :: Script k v a -> (a -> Script k v b) -> Script k v b

instance Get (Script k v) k v where
    getValue = GetValue

instance Functor (Script k v) where
    fmap = Ap . Pure

instance Applicative (Script k v) where
    pure  = Pure
    (<*>) = Ap

instance Monad (Script k v) where
    return = Pure
    (>>)   = (*>)
    (>>=)  = Bind

getScript :: Compute Monad k v -> k -> Script k v (Maybe v)
getScript compute = compute GetValue

runScript :: Monad m => (k -> m v) -> Script k v a -> m a
runScript get script = case script of
    GetValue k -> get k
    Pure v     -> pure v
    Ap s1 s2   -> runScript get s1 <*> runScript get s2
    Bind s f   -> runScript get s >>= fmap (runScript get) f

-- TODO: Fix inifinite loop
staticScriptDependencies :: Script k v a -> [k]
staticScriptDependencies script = case script of
    GetValue k -> [k]
    Pure _     -> []
    Ap s1 s2   -> staticScriptDependencies s1 ++ staticScriptDependencies s2
    Bind s _   -> staticScriptDependencies s

isStatic :: Script k v a -> Bool
isStatic script = case script of
    GetValue _ -> True
    Pure _     -> True
    Ap s1 s2   -> isStatic s1 && isStatic s2
    Bind _ _   -> False

isInput :: Eq k => Script k v a -> k -> Bool
isInput script key = case script of
    GetValue k -> k == key
    Pure _     -> True
    Ap s1 s2   -> isInput s1 key && isInput s2 key
    Bind _ _   -> False
