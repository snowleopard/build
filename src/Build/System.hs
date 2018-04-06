{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Build.System (
    -- * Toy build systems
    dumb, busy, memo,

    -- * Applicative build systems
    make, ninja, bazel, buck,

    -- * Monadic build systems
    excel, shake, cloudShake --, nix
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
        Just task -> do v <- run task fetch; modify (putValue k v); return v

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

-------------------------------------- Nix -------------------------------------
-- nix :: (Hashable k, Hashable v) => Build Monad (DCT k v) k v
-- nix = recursive $ \key fetch act -> do
--     ctd <- gets (getInfo . fst)
--     let deps = [] -- Here is the tricky part: we need to store this in CTD
--     dirty <- not <$> verifyCTD (fmap hash . fetch) key deps ctd
--     when dirty $ do
--         maybeValue <- constructCTD (fmap hash . fetch) key deps ctd
--         case maybeValue of
--             Just value -> modify $ \(s, t) -> (putValue key value s, t)
--             Nothing -> do
--                 (value, deps) <- act
--                 modify $ \(s, t) ->
--                     let newS = putValue key value s
--                     in (mapInfo (recordCTD newS key deps <>) newS, t)
