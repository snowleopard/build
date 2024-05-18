{-# LANGUAGE ImpredicativeTypes, FlexibleContexts, ScopedTypeVariables #-}

-- | Models of several build systems.
module Build.System (
    -- * Toy build systems
    dumb, busy, memo,

    -- * Applicative build systems
    make, ninja, cloudBuild, buck,

    -- * Monadic build systems
    excel, shake, cloudShake, bazel, nix
    ) where

import Control.Monad.State

import Build
import Build.Scheduler
import Build.Store
import Build.Rebuilder
import Build.Trace

-- | This is not a correct build system: given a target key, it simply rebuilds
-- it, without rebuilding any of its dependencies.
dumb :: Eq k => Build Monad () k v
dumb = independent perpetualRebuilder

-- | This is a correct but non-minimal build system: given a target key it
-- recursively rebuilds its dependencies, even if they are already up to date.
-- There is no memoisation, therefore a key may be built multiple times.
busy :: forall k v. Eq k => Build Monad () k v
busy tasks key = execState (fetch key)
  where
    fetch :: k -> State (Store () k v) v
    fetch k = case tasks k of
        Nothing   -> gets (getValue k)
        Just task -> do v <- task fetch; modify (putValue k v); return v

-- | This is a correct but non-minimal build system: it will rebuild keys even
-- if they are up to date. However, it performs memoization, therefore it never
-- builds a key twice.
memo :: Ord k => Build Monad () k v
memo = suspending perpetualRebuilder

-- | A model of Make: an applicative build system that uses file modification
-- times to check if a key is up to date.
make :: Ord k => Build Applicative (MakeInfo k) k v
make = topological modTimeRebuilder

-- | A model of Ninja: an applicative build system that uses verifying traces
-- to check if a key is up to date.
ninja :: forall k v. (Ord k, Hashable v) => Build Applicative (VT k v) k v
ninja = topological rebuilder
  where
    rebuilder :: Rebuilder Applicative (VT k v) k v
    rebuilder = vtRebuilder

-- | Excel stores a dirty bit per key and a calc chain.
type ExcelInfo k = (k -> Bool, Chain k)

-- | A model of Excel: a monadic build system that stores the calculation chain
-- from the previous build and approximate dependencies.
excel :: Ord k => Build Monad (ExcelInfo k) k v
excel = restarting dirtyBitRebuilder

-- | A model of Shake: a monadic build system that uses verifying traces to
-- check if a key is up to date.
shake :: (Ord k, Hashable v) => Build Monad (VT k v) k v
shake = suspending vtRebuilder

-- | A model of Bazel: a monadic build system that uses constructive traces
-- to check if a key is up to date as well as for caching build results. Note
-- that Bazel currently does not allow users to write monadic build rules: only
-- built-in rules have access to dynamic dependencies.
bazel :: (Ord k, Hashable v) => Build Monad (CT k v) k v
bazel = restarting2 ctRebuilder

-- | A model of Cloud Shake: a monadic build system that uses constructive
-- traces to check if a key is up to date as well as for caching build results.
cloudShake :: (Ord k, Hashable v) => Build Monad (CT k v) k v
cloudShake = suspending ctRebuilder

-- | A model of CloudBuild: an applicative build system that uses constructive
-- traces to check if a key is up to date as well as for caching build results.
cloudBuild :: forall k v. (Ord k, Hashable v) => Build Applicative (CT k v) k v
cloudBuild = topological rebuilder
  where
    rebuilder :: Rebuilder Applicative (CT k v) k v
    rebuilder = ctRebuilder

-- | A model of Buck: an applicative build system that uses deep constructive
-- traces to check if a key is up to date as well as for caching build results.
buck :: forall k v. (Ord k, Hashable v) => Build Applicative (DCT k v) k v
buck = topological rebuilder
  where
    rebuilder :: Rebuilder Applicative (DCT k v) k v
    rebuilder = dctRebuilder

-- | A model of Nix: a monadic build system that uses deep constructive traces
-- to check if a key is up to date as well as for caching build results.
nix :: (Ord k, Hashable v) => Build Monad (DCT k v) k v
nix = suspending dctRebuilder
