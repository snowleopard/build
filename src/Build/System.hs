{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Build.System (
    -- * Toy build systems
    dumb, busy, memo,

    -- * Applicative build systems
    make, ninja, bazel, buck,

    -- * Monadic build systems
    excel, shake, cloudShake, nix
    ) where

import Control.Monad.State

import Build
import Build.Scheduler
import Build.Store
import Build.Rebuilder
import Build.Trace

-- Not a correct build system
dumb :: Eq k => Build Monad () k v
dumb = independent perpetualRebuilder

-- Not a minimal build system
busy :: forall k v. Eq k => Build Monad () k v
busy tasks key store = execState (fetch key) store
  where
    fetch :: k -> State (Store () k v) v
    fetch k = case tasks k of
        Nothing   -> gets (getValue k)
        Just task -> do v <- task fetch; modify (putValue k v); return v

-- Not a minimal build system, but never builds a key twice
memo :: Eq k => Build Monad () k v
memo = recursive perpetualRebuilder

make :: forall k v. Ord k => Build Applicative (MakeInfo k) k v
make = topological modTimeRebuilder

ninja :: (Ord k, Hashable v) => Build Applicative (VT k v) k v
ninja = topological vtRebuilder

type ExcelInfo k = (ApproximationInfo k, Chain k)

excel :: Ord k => Build Monad (ExcelInfo k) k v
excel = reordering approximationRebuilder

shake :: (Eq k, Hashable v) => Build Monad (VT k v) k v
shake = recursive vtRebuilder

bazel :: (Ord k, Hashable v) => Build Applicative (CT k v) k v
bazel = topological ctRebuilder

cloudShake :: (Eq k, Hashable v) => Build Monad (CT k v) k v
cloudShake = recursive ctRebuilder

buck :: (Hashable k, Hashable v) => Build Applicative (DCT k v) k v
buck = topological dctRebuilder

nix :: (Hashable k, Hashable v) => Build Monad (DCT k v) k v
nix = recursive dctRebuilder
