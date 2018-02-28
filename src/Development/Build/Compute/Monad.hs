{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Monad (
    Script (..), getScript, run, runWith, staticDependencies, isStatic, isInput
    ) where

import Data.Monoid
import Development.Build.Store

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
    (>>=)  = Bind

getScript :: (forall m. (Monad m, Get m k v) => k -> m v) -> k -> Script k v v
getScript = id

run :: (Monad m, Get m k v) => Script k v a -> m a
run = runWith getValue

runWith :: Monad m => (k -> m v) -> Script k v a -> m a
runWith get script = case script of
    GetValue k -> get k
    Pure v     -> pure v
    Ap s1 s2   -> runWith get s1 <*> runWith get s2
    Bind s f   -> runWith get s >>= fmap (runWith get) f

newtype StaticConst c a = StaticConst { getStaticConst :: c }

instance Functor (StaticConst c) where
    fmap _ (StaticConst c) = StaticConst c

instance Monoid c => Applicative (StaticConst c) where
    pure _                = StaticConst mempty
    StaticConst x <*> StaticConst y = StaticConst (x <> y)

instance Monoid c => Monad (StaticConst c) where
    return              = pure
    StaticConst x >>= _ = StaticConst x

staticDependencies :: Script k v a -> [k]
staticDependencies = getStaticConst . runWith (StaticConst . return)

isStatic :: Script k v a -> Bool
isStatic script = case script of
    GetValue _ -> True
    Pure _     -> True
    Ap s1 s2   -> isStatic s1 && isStatic s2
    Bind _ _   -> False

isInput :: Eq k => (forall m. (Monad m, Get m k v) => k -> m v) -> k -> Bool
isInput compute key = isStatic script && case staticDependencies script of
    []    -> True
    [dep] -> dep == key
    _     -> False
  where
    script = getScript compute key
