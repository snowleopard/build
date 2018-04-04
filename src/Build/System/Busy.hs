{-# LANGUAGE ScopedTypeVariables #-}
module Build.System.Busy (busy, memo) where

import Control.Monad.State

import Build
import Build.Algorithm
import Build.Store

busy :: forall k v. Eq k => Build Monad () k v
busy task key store = execState (fetch key) store
  where
    fetch :: k -> State (Store () k v) v
    fetch k = case task fetch k of
        Nothing  -> gets (getValue k)
        Just act -> do v <- act; modify (putValue k v); return v

memo :: Eq k => Build Monad () k v
memo = recursive $ \key _fetch act -> do
    (value, _deps) <- act
    modify $ \(store, t) -> (putValue key value store, t)
