{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications, GeneralizedNewtypeDeriving #-}
module Build.Scheduler (
    topological,
    reordering, Chain, reordering2,
    restarting,
    recursive,
    independent
    ) where

import Control.Monad.State
import Data.Set (Set)

import Build
import Build.Task
import Build.Task.Monad
import Build.Task.Wrapped
import Build.Store
import Build.Rebuilder
import Build.Utilities

import qualified Data.Set               as Set
import qualified Build.Task.Applicative as A

-- | Update the value of a key in the store. The function takes both the current
-- value (the first parameter of type @v@) and the new value (the second
-- parameter of type @v@), and can potentially avoid touching the store if the
-- value is unchanged. The current implementation simply ignores the current
-- value, but in future this may be optimised, e.g. by comparing their hashes.
updateValue :: Eq k => k -> v -> v -> Store i k v -> Store i k v
updateValue key _value newValue = putValue key newValue

---------------------------------- Topological ---------------------------------
-- | This scheduler builds the dependency graph of the target key by extracting
-- all (static) dependencies upfront, and then traversing the graph in the
-- topological order, rebuilding keys using the supplied rebuilder.
topological :: Ord k => Rebuilder Applicative i k v -> Build Applicative i k v
topological rebuilder tasks key = execState $ forM_ chain $ \k ->
    case tasks k of
        Nothing   -> return ()
        Just task -> do
            value <- gets (getValue k)
            let newTask = rebuilder k value (unwrap @Applicative task)
                newFetch :: k -> StateT i (State (Store i k v)) v
                newFetch = lift . gets . getValue
            info <- gets getInfo
            (newValue, newInfo) <- runStateT (newTask newFetch) info
            modify $ putInfo newInfo . updateValue k value newValue
  where
    deps  = maybe [] (\t -> A.dependencies $ unwrap @Applicative t) . tasks
    chain = case topSort (graph deps key) of
        Nothing -> error "Cannot build tasks with cyclic dependencies"
        Just xs -> xs

---------------------------------- Restarting ----------------------------------
-- | The so-called @calculation chain@: the order in which keys were built
-- during the previous build, which is used as the best guess for the current
-- build by Excel and other similar build systems.
type Chain k = [k]

reordering :: forall i k v. Ord k => PartialRebuilder Monad i k v -> Build Monad (i, Chain k) k v
reordering rebuilder tasks key = execState $ do
    chain    <- snd . getInfo <$> get
    newChain <- go $ chain ++ [key | key `notElem` chain]
    modify . mapInfo $ \(i, _) -> (i, newChain)
  where
    go :: Chain k -> State (Store (i, Chain k) k v) (Chain k)
    go []     = return []
    go (k:ks) = case tasks k of
        Nothing -> (k :) <$> go ks
        Just task -> do
            store <- get
            let value   = getValue k store
                newTask = rebuilder k value (unwrap @Monad task)
                fetch k = return $ Right (getValue k store)
                (result, newInfo) = runState (newTask fetch) (fst $ getInfo store)
            case result of
                Left dep -> go $ [ dep | dep `notElem` ks ] ++ ks ++ [k]
                Right newValue -> do
                    modify $ putInfo (newInfo, []) . updateValue k value newValue
                    (k :) <$> go ks

-- | A model of the scheduler used by Excel, which builds keys in the order used
-- in the previous build. If a key cannot be build because its dependencies have
-- changed and a new dependency is still dirty, the corresponding build task is
-- abandoned and the key is moved at the end of the calculation chain, so it can
-- be restarted when all its dependencies are up to date.
reordering2 :: forall i k v. Ord k => Rebuilder Monad i k v -> Build Monad (i, Chain k) k v
reordering2 rebuilder tasks key = execState $ do
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
                store <- get
                let value = getValue k store
                    newTask :: Task (MonadState i) k v
                    newTask = rebuilder k value (unwrap @Monad task)
                    newFetch :: k -> State i (Either k v)
                    newFetch k | k `Set.member` done = return $ Right (getValue k store)
                               | otherwise = return (Left k)
                    info = fst $ getInfo store
                    (result, newInfo) = runState (trying newTask newFetch) info
                case result of
                    Left dep -> go done $ [ dep | dep `notElem` ks ] ++ ks ++ [k]
                    Right newValue -> do
                        modify $ putInfo (newInfo, []) . updateValue k value newValue
                        (k :) <$> go (Set.insert k done) ks

-- An item in the queue comprises a key that needs to be built and a list of
-- keys that are blocked on it. More efficient implementations are possible,
-- e.g. storing blocked keys in a @Map k [k]@ would allow faster queue updates.
type Queue k = [(k, [k])]

enqueue :: Eq k => k -> [k] -> Queue k -> Queue k
enqueue key blocked [] = [(key, blocked)]
enqueue key blocked ((k, bs):q)
    | k == key  = (k, blocked ++ bs) : q
    | otherwise = (k, bs) : enqueue key blocked q

dequeue :: Queue k -> Maybe (k, [k], Queue k)
dequeue []          = Nothing
dequeue ((k, bs):q) = Just (k, bs, q)

type IsDirty i k v = k -> Store i k v -> Bool

restarting :: forall i k v. Eq k => IsDirty i k v -> Rebuilder Monad i k v -> Build Monad i k v
restarting isDirty rebuilder tasks key = execState $ go (enqueue key [] mempty)
  where
    go :: Queue k -> State (Store i k v) ()
    go queue = case dequeue queue of
        Nothing -> return ()
        Just (k, bs, q) -> case tasks k of
            Nothing -> return () -- Never happens: we have no inputs in the queue
            Just task -> do
                store <- get
                let value = getValue k store
                    upToDate k = isInput tasks k || not (isDirty k store)
                    newTask :: Task (MonadState i) k v
                    newTask = rebuilder k value (unwrap @Monad task)
                    newFetch :: k -> State i (Either k v)
                    newFetch k | upToDate k = return (Right (getValue k store))
                               | otherwise  = return (Left k)
                    (result, newInfo) = runState (trying newTask newFetch) (getInfo store)
                case result of
                    Left dirtyDependency -> go (enqueue dirtyDependency (k:bs) q)
                    Right newValue -> do
                        modify $ putInfo newInfo . updateValue k value newValue
                        go (foldr (\b -> enqueue b []) q bs)

----------------------------------- Recursive ----------------------------------
recursive :: forall i k v. Eq k => Rebuilder Monad i k v -> Build Monad i k v
recursive rebuilder tasks key store = fst $ execState (fetch key) (store, [])
  where
    fetch :: k -> State (Store i k v, [k]) v
    fetch key = case tasks key of
        Nothing -> gets (getValue key . fst)
        Just task -> do
            done <- gets snd
            when (key `notElem` done) $ do
                value <- gets (getValue key . fst)
                let newTask = rebuilder key value (unwrap @Monad task)
                    newFetch :: k -> StateT i (State (Store i k v, [k])) v
                    newFetch = lift . fetch
                info <- gets (getInfo . fst)
                (newValue, newInfo) <- runStateT (newTask newFetch) info
                modify $ \(s, done) ->
                    (putInfo newInfo $ updateValue key value newValue s, done)
            gets (getValue key . fst)

-- | An incorrect scheduler that builds the target key without respecting its
-- dependencies. It produces the correct result only if all dependencies of the
-- target key are up to date.
independent :: forall i k v. Eq k => Rebuilder Monad i k v -> Build Monad i k v
independent rebuilder tasks key store = case tasks key of
    Nothing -> store
    Just task ->
        let value   = getValue key store
            newTask = rebuilder key value (unwrap @Monad task)
            newFetch :: k -> State i v
            newFetch k = return (getValue k store)
            (newValue, newInfo) = runState (newTask newFetch) (getInfo store)
        in putInfo newInfo $ updateValue key value newValue store
