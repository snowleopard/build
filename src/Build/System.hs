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
import Build.Algorithm
import Build.Store
import Build.Strategy
import Build.Task
import Build.Trace

-- Not a correct build system
dumb :: Eq k => Build Monad () k v
dumb = independent alwaysRebuildStrategy

-- Not a minimal build system
busy :: forall k v. Eq k => Build Monad () k v
busy tasks key store = execState (fetch key) store
  where
    fetch :: k -> State (Store () k v) v
    fetch k = case tasks k of
        Nothing   -> gets (getValue k)
        Just w -> do v <- unwrap w fetch; modify (putValue k v); return v

-- Not a minimal build system, but never builds a key twice
memo :: Eq k => Build Monad () k v
memo = recursive alwaysRebuildStrategy

make :: forall k v. Ord k => Build Applicative (MakeInfo k) k v
make = topological makeStrategy

ninja :: (Ord k, Hashable v) => Build Applicative (VT k v) k v
ninja = topological vtStrategyA

type ExcelInfo k = (ApproximationInfo k, Chain k)

excel :: Ord k => Build Monad (ExcelInfo k) k v
excel = reordering approximationStrategy

shake :: (Eq k, Hashable v) => Build Monad (VT k v) k v
shake = recursive vtStrategyM

bazel :: (Ord k, Hashable v) => Build Applicative (CT k v) k v
bazel = topological ctStrategyA

cloudShake :: (Eq k, Hashable v) => Build Monad (CT k v) k v
cloudShake = recursive ctStrategyM

buck :: (Hashable k, Hashable v) => Build Applicative (DCT k v) k v
buck = topological dctStrategyA

nix :: (Hashable k, Hashable v) => Build Monad (DCT k v) k v
nix = recursive dctStrategyM