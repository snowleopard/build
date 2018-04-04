{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TupleSections #-}
module Build.Algorithm where

import Control.Monad.State
import Data.Set (Set)

import Build
import Build.Task
import Build.Task.Applicative hiding (exceptional)
import Build.Task.Monad hiding (dependencies)
import Build.Store
import Build.Utilities

import Data.Map (Map)
import Control.Monad.Extra

import qualified Data.Set as Set

data Trace k v = Trace
    { key          :: k
    , depends :: [(k, Hash v)]
    , result       :: Hash v }

-- Determine whether a trace is relevant to the current state
traceMatch :: (Monad m, Eq k) => (k -> Hash v -> m Bool) -> k -> [Trace k v] -> m [Hash v]
traceMatch check key ts = mapMaybeM f ts
    where f (Trace k dkv v) = do
                b <- return (key == k) &&^ allM (uncurry check) dkv
                return $ if b then Just v else Nothing

data Traces k v = Traces
    { traces :: [Trace k v]
    , contents  :: Map (Hash v) v }

topological :: Ord k
            => (k -> [k] -> State (Store i k v) v -> State (Store i k v) ())
            -> Build Applicative i k v
topological process task key = execState $ forM_ chain $ \k -> do
    let fetch k = gets (getValue k)
    case task fetch k of
        Nothing  -> return ()
        Just act -> process k (deps k) act
  where
    deps  = dependencies task
    chain = case topSort (graph deps key) of
        Nothing -> error "Cannot build tasks with cyclic dependencies"
        Just xs -> xs

data Result k v = MissingDependency k | Result v [k]

try :: forall m k v. Monad m => Task Monad k v -> (k -> m (Maybe v))
                           -> k -> Maybe (m (Result k v))
try task partialFetch = fmap (fmap toResult) . trackExceptions task partialFetch
  where
    toResult (Left k       ) = MissingDependency k
    toResult (Right (v, ks)) = Result v ks

type CalcChain k = [k]

reordering :: forall i k v. Ord k
            => (k -> State (Store i k v) (Result k v) -> State (Store i k v) (Maybe (Result k v)))
            -> Build Monad (i, CalcChain k) k v
reordering step task key = execState $ do
    chain    <- snd . getInfo <$> get
    newChain <- go Set.empty $ chain ++ [key | key `notElem` chain]
    modify $ \s -> putInfo (fst (getInfo s), newChain) s
  where
    go :: Set k -> CalcChain k -> State (Store (i, [k]) k v) (CalcChain k)
    go _    []     = return []
    go done (k:ks) = do
        case try task fetch k of
            Nothing -> (k :) <$> go (Set.insert k done) ks
            Just act -> do
                store <- get
                let (res, newStore) = runState (step k act) (mapInfo fst store)
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
          => (k -> (k -> State (Store i k v, [k]) v)
                -> State (Store i k v, [k]) (v, [k])
                -> State (Store i k v, [k]) ())
          -> Build Monad i k v
recursive process task key store = fst $ execState (ensure key) (store, [])
  where
    ensure :: k -> State (Store i k v, [k]) ()
    ensure key = do
        let fetch k = do ensure k; gets (getValue k . fst)
        done <- gets snd
        when (key `notElem` done) $ do
            modify $ \(s, done) -> (s, key:done)
            case trackM task fetch key of
                Nothing -> return ()
                Just act -> process key fetch act
