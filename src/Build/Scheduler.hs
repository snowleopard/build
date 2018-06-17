{-# LANGUAGE ScopedTypeVariables #-}

-- | Build schedulers execute task rebuilders in the right order.
module Build.Scheduler (
    topological,
    restarting, Chain,
    restartingB, restarting2,
    suspending,
    independent
    ) where

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Set (Set)

import Build
import Build.Task
import Build.Task.Applicative
import Build.Task.Monad
import Build.Trace
import Build.Store
import Build.Rebuilder
import Build.Utilities

import qualified Data.Set as Set

-- | Update the value of a key in the store. The function takes both the current
-- value (the first parameter of type @v@) and the new value (the second
-- parameter of type @v@), and can potentially avoid touching the store if the
-- value is unchanged. The current implementation simply ignores the current
-- value, but in future this may be optimised, e.g. by comparing their hashes.
updateValue :: Eq k => k -> v -> v -> Store i k v -> Store i k v
updateValue key _value newValue = putValue key newValue

---------------------------------- Topological ---------------------------------
-- | This scheduler constructs the dependency graph of the target key by
-- extracting all (static) dependencies upfront, and then traversing the graph
-- in the topological order, rebuilding keys using the supplied rebuilder.
topological :: forall i k v. Ord k => Rebuilder Applicative i k v -> Build Applicative i k v
topological rebuilder tasks target = execState $ forM_ order $ \key -> case tasks key of
    Nothing -> return ()
    Just task -> do
        store <- get
        let value = getValue key store
            newTask :: Task (MonadState i) k v
            newTask = rebuilder key value task
            fetch :: k -> State i v
            fetch k = pure (getValue k store)
            (newValue, newInfo) = runState (run newTask fetch) (getInfo store)
        modify $ putInfo newInfo . updateValue key value newValue
  where
    deps k = case tasks k of { Nothing -> []; Just task -> dependencies task }
    order  = case topSort (graph deps target) of
        Nothing -> error "Cannot build tasks with cyclic dependencies"
        Just xs -> xs

---------------------------------- Restarting ----------------------------------
-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the task from the type of values @v@ to @Either e v@,
-- where the result @Left e@ indicates that the task failed, e.g. because of a
-- failed dependency lookup, and @Right v@ yeilds the value otherwise.
try :: Task (MonadState i) k v -> Task (MonadState i) k (Either e v)
try task = Task $ \fetch -> runExceptT $ run task (ExceptT . fetch)

-- | The so-called @calculation chain@: the order in which keys were built
-- during the previous build, which is used as the best guess for the current
-- build by Excel and other similar build systems.
type Chain k = [k]

-- | A model of the scheduler used by Excel, which builds keys in the order used
-- in the previous build. If a key cannot be build because its dependencies have
-- changed and a new dependency is still dirty, the corresponding build task is
-- abandoned and the key is moved at the end of the calculation chain, so it can
-- be restarted when all its dependencies are up to date.
restarting :: forall i k v. Ord k => Rebuilder Monad i k v -> Build Monad (i, Chain k) k v
restarting rebuilder tasks target = execState $ do
    chain    <- gets (snd . getInfo)
    newChain <- go Set.empty $ chain ++ [target | target `notElem` chain]
    modify . mapInfo $ \(i, _) -> (i, newChain)
  where
    go :: Set k -> Chain k -> State (Store (i, Chain k) k v) (Chain k)
    go _    []       = return []
    go done (key:ks) = case tasks key of
        Nothing -> (key :) <$> go (Set.insert key done) ks
        Just task -> do
            store <- get
            let value = getValue key store
                newTask :: Task (MonadState i) k (Either k v)
                newTask = try $ rebuilder key value task
                fetch :: k -> State i (Either k v)
                fetch k | k `Set.member` done = return $ Right (getValue k store)
                        | otherwise           = return $ Left k
            case runState (run newTask fetch) (fst $ getInfo store) of
                (Left dep, _) -> go done $ [ dep | dep `notElem` ks ] ++ ks ++ [key]
                (Right newValue, newInfo) -> do
                    modify $ putInfo (newInfo, []) . updateValue key value newValue
                    (key :) <$> go (Set.insert key done) ks

-- | An item in the queue comprises a key that needs to be built and a list of
-- keys that are blocked on it. More efficient implementations are possible,
-- e.g. storing blocked keys in a @Map k [k]@ would allow faster queue updates.
type Queue k = [(k, [k])]

-- | Add a key with a list of blocked keys to the queue. If the key is already
-- in the queue, extend its list of blocked keys.
enqueue :: Eq k => k -> [k] -> Queue k -> Queue k
enqueue key blocked [] = [(key, blocked)]
enqueue key blocked ((k, bs):q)
    | k == key  = (k, blocked ++ bs) : q
    | otherwise = (k, bs) : enqueue key blocked q

-- | Extract a key and a list of blocked keys from the queue, or return
-- @Nothing@ if the queue is empty.
dequeue :: Queue k -> Maybe (k, [k], Queue k)
dequeue []          = Nothing
dequeue ((k, bs):q) = Just (k, bs, q)

-- | Check if a key is dirty by examining its dependencies, as well as the
-- stored build information.
type IsDirty i k v = k -> Store i k v -> Bool

-- | A model of the scheduler used by Bazel. We extract a key K from the queue
-- and try to build it. There are now two cases:
-- 1. The build fails because one of the dependencies of K is dirty. In this
--    case we add the dirty dependency to the queue, listing K as blocked by it.
-- 2. The build succeeds, in which case we add all keys that were previously
--    blocked by K to the queue.
restartingB :: forall i k v. Eq k => IsDirty i k v -> Rebuilder Monad i k v -> Build Monad i k v
restartingB isDirty rebuilder tasks target = execState $ go (enqueue target [] mempty)
  where
    go :: Queue k -> State (Store i k v) ()
    go queue = case dequeue queue of
        Nothing -> return ()
        Just (key, bs, q) -> case tasks key of
            Nothing -> return () -- Never happens: we have no inputs in the queue
            Just task -> do
                store <- get
                let value = getValue key store
                    upToDate k = isInput tasks k || not (isDirty k store)
                    newTask :: Task (MonadState i) k (Either k v)
                    newTask = try $ rebuilder key value task
                    fetch :: k -> State i (Either k v)
                    fetch k | upToDate k = return (Right (getValue k store))
                            | otherwise  = return (Left k)
                case runState (run newTask fetch) (getInfo store) of
                    (Left dep, _) -> go (enqueue dep (key:bs) q)
                    (Right newValue, newInfo) -> do
                        modify $ putInfo newInfo . updateValue key value newValue
                        go (foldr (\b -> enqueue b []) q bs)

-- | A model of the scheduler used by Bazel, specialised to constructive traces.
restarting2 :: (Hashable v, Eq k) => Rebuilder Monad (CT k v) k v -> Build Monad (CT k v) k v
restarting2 = restartingB isDirtyCT

---------------------------------- Suspending ----------------------------------
-- | This scheduler builds keys recursively: to build a key it executes the
-- associated task, discovering its dependencies on the fly, and if one of the
-- dependencies is dirty, the task is suspended until the dependency is rebuilt.
-- It stores the set of keys that have already been built as part of the state
-- to avoid executing the same task twice.
suspending :: forall i k v. Ord k => Rebuilder Monad i k v -> Build Monad i k v
suspending rebuilder tasks target store = fst $ execState (build target) (store, Set.empty)
  where
    build :: k -> State (Store i k v, Set k) ()
    build key = case tasks key of
        Nothing -> return ()
        Just task -> do
            done <- gets snd
            when (key `Set.notMember` done) $ do
                value <- gets (getValue key . fst)
                let newTask :: Task (MonadState i) k v
                    newTask = rebuilder key value task
                    fetch :: k -> StateT i (State (Store i k v, Set k)) v
                    fetch k = do lift (build k)                      -- build the key
                                 lift (gets (getInfo . fst)) >>= put -- save new traces
                                 lift (gets (getValue k . fst))      -- fetch the value
                info <- gets (getInfo . fst)
                (newValue, newInfo) <- runStateT (run newTask fetch) info
                modify $ \(s, _) ->
                    ( putInfo newInfo $ updateValue key value newValue s
                    , Set.insert key done )

-- | An incorrect scheduler that builds the target key without respecting its
-- dependencies. It produces the correct result only if all dependencies of the
-- target key are up to date.
independent :: forall i k v. Eq k => Rebuilder Monad i k v -> Build Monad i k v
independent rebuilder tasks target store = case tasks target of
    Nothing -> store
    Just task ->
        let value   = getValue target store
            newTask = rebuilder target value task
            fetch :: k -> State i v
            fetch k = return (getValue k store)
            (newValue, newInfo) = runState (run newTask fetch) (getInfo store)
        in putInfo newInfo $ updateValue target value newValue store
