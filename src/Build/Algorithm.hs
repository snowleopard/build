{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TupleSections #-}
module Build.Algorithm (
    topological,
    reordering, Chain,
    recursive,
    independent
    ) where

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Set (Set)

import Build
import Build.Task
import Build.Task.Applicative hiding (unwrap, exceptional)
import Build.Store
import Build.Strategy
import Build.Utilities

import qualified Data.Set as Set
import qualified Build.Task.Applicative as A
import qualified Build.Task.Monad as M

-- Shall we skip writing to the store if the value is the same?
-- We could skip writing if hash value == hash newValue.
updateValue :: Eq k => k -> v -> v -> Store i k v -> Store i k v
updateValue key _value newValue = putValue key newValue

---------------------------------- Topological ---------------------------------
topological :: Ord k => Strategy Applicative i k v -> Build Applicative i k v
topological strategy tasks key = execState $ forM_ chain $ \k ->
    case tasks k of
        Nothing   -> return ()
        Just task -> do
            value <- gets (getValue k)
            let newTask = strategy k value (A.unwrap task)
                newFetch :: k -> StateT i (State (Store i k v)) v
                newFetch = lift . gets . getValue
            info <- gets getInfo
            (newValue, newInfo) <- runStateT (newTask newFetch) info
            modify $ putInfo newInfo . updateValue k value newValue
  where
    deps  = maybe [] (\t -> dependencies (A.unwrap t)) . tasks
    chain = case topSort (graph deps key) of
        Nothing -> error "Cannot build tasks with cyclic dependencies"
        Just xs -> xs

---------------------------------- Reordering ----------------------------------
type Chain k = [k]

trying :: Task (MonadState i) k v -> Task (MonadState i) k (Either e v)
trying task fetch = runExceptT $ task (ExceptT . fetch)

reordering :: forall i k v. Ord k => Strategy Monad i k v -> Build Monad (i, Chain k) k v
reordering strategy tasks key = execState $ do
    chain    <- snd . getInfo <$> get
    newChain <- go Set.empty $ chain ++ [key | key `notElem` chain]
    modify . mapInfo $ \(i, _) -> (i, newChain)
  where
    go :: Set k -> Chain k -> State (Store (i, [k]) k v) (Chain k)
    go _    []     = return []
    go done (k:ks) = do
        case tasks k of
            Nothing -> (k :) <$> go (Set.insert k done) ks
            Just task -> do
                value <- gets (getValue k)
                let newTask :: Task (MonadState i) k v
                    newTask = strategy k value (M.unwrap task)
                    newFetch :: k -> StateT i (State (Store (i, [k]) k v)) (Either k v)
                    newFetch k | k `Set.member` done = do
                                   store <- lift get
                                   return $ Right (getValue k store)
                               | otherwise = return (Left k)
                info <- fst <$> gets getInfo
                (result, newInfo) <- runStateT (trying newTask newFetch) info
                case result of
                    Left dep -> go done $ [ dep | dep `notElem` ks ] ++ ks ++ [k]
                    Right newValue -> do
                        modify $ putInfo (newInfo, []) . updateValue k value newValue
                        (k :) <$> go (Set.insert k done) ks

----------------------------------- Recursive ----------------------------------
recursive :: forall i k v. Eq k => Strategy Monad i k v -> Build Monad i k v
recursive strategy tasks key store = fst $ execState (fetch key) (store, [])
  where
    fetch :: k -> State (Store i k v, [k]) v
    fetch key = case tasks key of
        Nothing -> gets (getValue key . fst)
        Just task -> do
            done <- gets snd
            when (key `notElem` done) $ do
                value <- gets (getValue key . fst)
                let newTask = strategy key value (M.unwrap task)
                    newFetch :: k -> StateT i (State (Store i k v, [k])) v
                    newFetch = lift . fetch
                info <- gets (getInfo . fst)
                (newValue, newInfo) <- runStateT (newTask newFetch) info
                modify $ \(s, done) ->
                    (putInfo newInfo $ updateValue key value newValue s, done)
            gets (getValue key . fst)

-- | An incorrect build algorithm that builds the target key without respecting
-- its dependencies. It produces the correct result only if all dependencies of
-- the target key are up to date.
independent :: forall i k v. Eq k => Strategy Monad i k v -> Build Monad i k v
independent strategy tasks key store = case tasks key of
    Nothing -> store
    Just task ->
        let value   = getValue key store
            newTask = strategy key value (M.unwrap task)
            newFetch :: k -> State i v
            newFetch k = return (getValue k store)
            (newValue, newInfo) = runState (newTask newFetch) (getInfo store)
        in putInfo newInfo $ updateValue key value newValue store
