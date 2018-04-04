{-# LANGUAGE ScopedTypeVariables #-}
module Build.System.Busy (busy, memo) where

import Control.Monad.State

import Build
import Build.Store

busy :: forall k v. Eq k => Build Monad () k v
busy task key store = execState (fetch key) store
  where
    fetch :: k -> State (Store () k v) v
    fetch k = case task fetch k of
        Nothing  -> gets (getValue k)
        Just act -> do v <- act; modify (putValue k v); return v

memo :: forall k v. Eq k => Build Monad () k v
memo task key store = fst $ execState (fetch key) (store, [])
  where
    fetch :: k -> State (Store () k v, [k]) v
    fetch k = case task fetch k of
        Nothing  -> gets (getValue k . fst)
        Just act -> do
            built <- snd <$> get
            when (k `notElem` built) $ do
                v <- act
                modify $ \(s, built) -> (putValue k v s, k : built)
            gets (getValue k . fst)
