{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TupleSections #-}
module Build.Algorithm (
    topological,
    reordering, CalcChain, Result(..),
    recursive
    ) where

import Control.Monad.State
import Data.Set (Set)

import Build
import Build.Task
import Build.Task.Applicative hiding (exceptional)
import Build.Task.Monad hiding (dependencies, inputs)
import Build.Store
import Build.Utilities

import qualified Data.Set as Set

topological :: Ord k => (k -> v -> Task Applicative k v -> Task (MonadState i) k v)
    -> Build Applicative i k v
topological transformer tasks key = execState $ forM_ chain $ \k ->
    case tasks k of
        Nothing   -> return ()
        Just task -> do
            currentValue <- gets (getValue k)
            let t = transformer k currentValue task
                fetch :: k -> StateT i (State (Store i k v)) v
                fetch = lift . gets . getValue
            info <- gets getInfo
            (value, newInfo) <- runStateT (run t fetch) info
            -- Shall we skip writing if the value is the same?
            modify $ putInfo newInfo . putValue k value
  where
    deps  = maybe [] dependencies . tasks
    chain = case topSort (graph deps key) of
        Nothing -> error "Cannot build tasks with cyclic dependencies"
        Just xs -> xs

data Result k v = MissingDependency k | Result v [k]

try :: forall m k v. Monad m => Task Monad k v -> (k -> m (Maybe v)) -> m (Result k v)
try task partialFetch = toResult <$> trackExceptions task partialFetch
  where
    toResult (Left k       ) = MissingDependency k
    toResult (Right (v, ks)) = Result v ks

type CalcChain k = [k]

reordering :: forall i k v. Ord k => (k -> State (Store i k v) (Result k v)
                                        -> State (Store i k v) (Maybe (Result k v)))
                                  -> Build Monad (i, CalcChain k) k v
reordering process tasks key = execState $ do
    chain    <- snd . getInfo <$> get
    newChain <- go Set.empty $ chain ++ [key | key `notElem` chain]
    modify . mapInfo $ \(i, _) -> (i, newChain)
  where
    go :: Set k -> CalcChain k -> State (Store (i, [k]) k v) (CalcChain k)
    go _    []     = return []
    go done (k:ks) = do
        case tasks k of
            Nothing -> (k :) <$> go (Set.insert k done) ks
            Just task -> do
                store <- get
                let act = try task fetch
                    (res, newStore) = runState (process k act) (mapInfo fst store)
                put $ mapInfo (,[]) newStore
                case res of
                    Just (MissingDependency d) -> go done $ [ d | d `notElem` ks ] ++ ks ++ [k]
                    _                          -> (k :) <$> go (Set.insert k done) ks
      where
        fetch :: k -> State (Store i k v) (Maybe v)
        fetch k | k `Set.member` done = gets (Just . getValue k)
                | otherwise           = return Nothing

-- Recursive dependency strategy
recursive :: forall i k v. Eq k
          => (k -> v -> Task Monad k v -> Task (MonadState i) k v)
          -> Build Monad i k v
recursive transformer tasks key store = fst $ execState (fetch key) (store, [])
  where
    fetch :: k -> State (Store i k v, [k]) v
    fetch key = case tasks key of
        Nothing -> gets (getValue key . fst)
        Just task -> do
            done <- gets snd
            when (key `notElem` done) $ do
                currentValue <- gets (getValue key . fst)
                let t = transformer key currentValue task
                    liftedFetch :: k -> StateT i (State (Store i k v, [k])) v
                    liftedFetch = lift . fetch
                info <- gets (getInfo . fst)
                (value, newInfo) <- runStateT (run t liftedFetch) info
                -- Shall we skip writing if the value is the same?
                modify $ \(s, done) -> (putInfo newInfo $ putValue key value s, done)
            gets (getValue key . fst)
