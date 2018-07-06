{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}

-- | Build schedulers execute task rebuilders in the right order.
module Build.Scheduler (
    topological,
    restarting, Chain,
    restarting2,
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

type Scheduler c i j k v = Rebuilder c j k v -> Build c i k v

-- | Lift a computation operating on @i@ to @Store i k v@.
liftStore :: State i a -> State (Store i k v) a
liftStore x = do (a, newInfo) <- gets (runState x . getInfo)
                 modify (putInfo newInfo)
                 return a

-- | Lift a computation operating on @Store i k v@ to @Store (i, j) k v@.
liftInfo :: State (Store i k v) a -> State (Store (i, j) k v) a
liftInfo x = do
    store <- get
    let (a, newStore) = runState x (mapInfo fst store)
    put $ mapInfo (, snd $ getInfo $ store) newStore
    return a

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
topological :: forall i k v. Ord k => Scheduler Applicative i i k v
topological rebuilder tasks target = execState $ mapM_ build order
  where
    build :: k -> State (Store i k v) ()
    build key = case tasks key of
        Nothing -> return ()
        Just task -> do
            store <- get
            let value = getValue key store
                newTask :: Task (MonadState i) k v
                newTask = rebuilder key value task
                fetch :: k -> State i v
                fetch k = return (getValue k store)
            newValue <- liftStore (run newTask fetch)
            modify $ putValue key newValue
    order = case topSort (graph deps target) of
        Nothing -> error "Cannot build tasks with cyclic dependencies"
        Just xs -> xs
    deps k = case tasks k of { Nothing -> []; Just task -> dependencies task }

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
restarting :: forall ir k v. Ord k => Scheduler Monad (ir, Chain k) ir k v
restarting rebuilder tasks target = execState $ do
    chain    <- gets (snd . getInfo)
    newChain <- liftInfo $ go Set.empty $ chain ++ [target | target `notElem` chain]
    modify . mapInfo $ \(ir, _) -> (ir, newChain)
  where
    go :: Set k -> Chain k -> State (Store ir k v) (Chain k)
    go _    []       = return []
    go done (key:ks) = case tasks key of
        Nothing -> (key :) <$> go (Set.insert key done) ks
        Just task -> do
            store <- get
            let value = getValue key store
                newTask :: Task (MonadState ir) k (Either k v)
                newTask = try $ rebuilder key value task
                fetch :: k -> State ir (Either k v)
                fetch k | k `Set.member` done = return $ Right (getValue k store)
                        | otherwise           = return $ Left k
            result <- liftStore (run newTask fetch)
            case result of
                Left dep -> go done $ dep : filter (/= dep) ks ++ [key]
                Right newValue -> do
                    modify $ updateValue key value newValue
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

-- TODO: The implementation below is specialised to constructive traces @CT@.
-- Can we make it polymorphic over the type of build information @i@ like other
-- schedulers?

-- | A model of the scheduler used by Bazel. We extract a key K from the queue
-- and try to build it. There are now two cases:
-- 1. The build fails because one of the dependencies of K is dirty. In this
--    case we add the dirty dependency to the queue, listing K as blocked by it.
-- 2. The build succeeds, in which case we add all keys that were previously
--    blocked by K to the queue.
restarting2 :: forall k v. (Hashable v, Eq k) => Scheduler Monad (CT k v) (CT k v) k v
restarting2 rebuilder tasks target = execState $ go (enqueue target [] mempty)
  where
    go :: Queue k -> State (Store (CT k v) k v) ()
    go queue = case dequeue queue of
        Nothing -> return ()
        Just (key, bs, q) -> case tasks key of
            Nothing -> return () -- Never happens: we have no inputs in the queue
            Just task -> do
                store <- get
                let value = getValue key store
                    upToDate k = isInput tasks k || not (isDirtyCT k store)
                    newTask :: Task (MonadState (CT k v)) k (Either k v)
                    newTask = try $ rebuilder key value task
                    fetch :: k -> State (CT k v) (Either k v)
                    fetch k | upToDate k = return (Right (getValue k store))
                            | otherwise  = return (Left k)
                result <- liftStore (run newTask fetch)
                case result of
                    Left dep -> go (enqueue dep (key:bs) q)
                    Right newValue -> do
                        modify $ updateValue key value newValue
                        go (foldr (\b -> enqueue b []) q bs)

---------------------------------- Suspending ----------------------------------
-- | This scheduler builds keys recursively: to build a key it executes the
-- associated task, discovering its dependencies on the fly, and if one of the
-- dependencies is dirty, the task is suspended until the dependency is rebuilt.
-- It stores the set of keys that have already been built as part of the state
-- to avoid executing the same task twice.
suspending :: forall i k v. Ord k => Scheduler Monad i i k v
suspending rebuilder tasks target store = fst $ execState (fetch target) (store, Set.empty)
  where
    fetch :: k -> State (Store i k v, Set k) v
    fetch key = do
        done <- gets snd
        case tasks key of
            Just task | key `Set.notMember` done -> do
                value <- gets (getValue key . fst)
                let newTask :: Task (MonadState i) k v
                    newTask = rebuilder key value task
                newValue <- liftRun newTask fetch
                modify $ \(s, d) -> (updateValue key value newValue s, Set.insert key d)
                return newValue
            _ -> gets (getValue key . fst) -- fetch the existing value


newtype Wrap i k v a = Wrap {unwrap :: State (Store i k v, Set k) a}
    deriving (Functor, Applicative, Monad)

instance MonadState i (Wrap i k v) where
    get = Wrap $ gets (getInfo . fst)
    put i = Wrap $ modify $ \(a, b) -> (putInfo i a, b)

liftRun :: Task (MonadState i) k v -> (k -> State (Store i k v, Set k) v) -> State (Store i k v, Set k) v
liftRun t f = unwrap $ run t (Wrap . f)



-- | An incorrect scheduler that builds the target key without respecting its
-- dependencies. It produces the correct result only if all dependencies of the
-- target key are up to date.
independent :: forall i k v. Eq k => Scheduler Monad i i k v
independent rebuilder tasks target store = case tasks target of
    Nothing -> store
    Just task ->
        let value   = getValue target store
            newTask = rebuilder target value task
            fetch :: k -> State i v
            fetch k = return (getValue k store)
            (newValue, newInfo) = runState (run newTask fetch) (getInfo store)
        in putInfo newInfo $ updateValue target value newValue store
