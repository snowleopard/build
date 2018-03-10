{-# LANGUAGE Rank2Types, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards, ConstraintKinds, TypeFamilies #-}

module Neil.Builder(
    dumb,
    dumbDynamic,
    dumbTopological,
    dumbRecursive,
    make,
    makeTrace,
    makeDirtyBit,
    shake,
    shakeDirtyBit,
    spreadsheet,
    spreadsheetTrace,
    spreadsheetRemote,
    bazel,
    shazel
    ) where

import Neil.Build
import Neil.Util
import Neil.Compute
import Control.Monad.Extra
import Data.Tuple.Extra
import Data.Default
import Data.Maybe
import Data.Either.Extra
import Debug.Trace
import Data.List
import Data.Typeable
import qualified Data.Set as Set
import qualified Data.Map as Map

---------------------------------------------------------------------
-- DEPENDENCY ORDER SCHEMES

topological :: Default i => (k -> [k] -> M i k v v -> M i k v ()) -> Build Applicative i k v
topological step compute k = runM $ do
    let depends = getDependencies compute
    forM_ (topSort depends $ transitiveClosure depends k) $ \k ->
        case compute getStore k of
            Nothing -> return ()
            Just act -> step k (depends k) act


newtype Recursive k = Recursive (Set.Set k)
    deriving Default

-- | Build a rule at most once in a single execution
recursive :: Default i => (k -> Maybe [k] -> (k -> M i k v v) -> M i k v ([k], v) -> M i k v ()) -> Build Monad i k v
recursive step compute = runM . ensure
    where
        ensure k = do
            let ask x = ensure x >> getStore x
            Recursive done <- getTemp
            when (k `Set.notMember` done) $ do
                modifyTemp $ \(Recursive set) -> Recursive $ Set.insert k set
                case trackDependencies compute ask k of
                    Nothing -> return ()
                    Just act -> step k (getDependenciesMaybe compute k) ask act


reordering
    :: (m ~ M (i, [k]) k v, Default i)
    => (k -> Maybe [k] -> (k -> m (Maybe v)) -> m (Either k ([k], v)) -> m (Maybe k)) -> Build Monad (i, [k]) k v
reordering step compute k = runM $ do
    order <- snd <$> getInfo
    order <- f Set.empty $ order ++ [k | k `notElem` order]
    modifyInfo $ second $ const order
    where
        f done [] = return []
        f done (k:ks) = do
            let f' x = if x `Set.member` done then Right <$> getStore x else return $ Left x
            case failDependencies compute f' k of
                Nothing -> (k :) <$> f (Set.insert k done) ks
                Just act -> do
                    res <- step k (getDependenciesMaybe compute k) (fmap eitherToMaybe . f') act
                    case res of
                        Nothing -> (k :) <$> f (Set.insert k done) ks
                        Just e -> f done $ [e | e `notElem` ks] ++ ks ++ [k]


---------------------------------------------------------------------
-- DIRTY SCHEMES

data Trace k v = Trace k [(k, Hash v)] (Hash v) deriving Show

-- | Given a way to ask for a dependency, and the key under construction, and the traces, pick the hashes that match
traces :: (Monad m, Eq k) => (k -> Hash v -> m Bool) -> k -> [Trace k v] -> m [Hash v]
traces check k ts = mapMaybeM f ts
    where f (Trace k2 ds v) = do
                b <- return (k == k2) &&^ allM (uncurry check) ds
                return $ if b then Just v else Nothing


---------------------------------------------------------------------
-- BUILD SYSTEMS


-- | Dumbest build system possible, always compute everything from scratch multiple times
dumb :: Build Monad () k v
dumb compute k = runM (f k)
    where f k = maybe (getStore k) (\act -> do v <- act; putStore k v; return v) $ compute f k

-- | Refinement of dumb, compute everything but at most once per execution
dumbRecursive :: Build Monad () k v
dumbRecursive = recursive $ \k _ _ act -> putStore k . snd =<< act

dumbTopological :: Build Applicative () k v
dumbTopological = topological $ \k _ act -> putStore k =<< act

dumbDynamic :: Build Monad ((), [k]) k v
dumbDynamic = reordering $ \k _ _ act -> do
    res <- act
    case res of
        Left e -> return $ Just e
        Right (_, v) -> do
            putStore k v
            return Nothing


-- | The simplified Make approach where we build a dependency graph and topological sort it
make :: Eq v => Build Applicative (Changed k v, ()) k v
make = withChangedApplicative $ topological $ \k ds act -> do
    kt <- getStoreTime k
    ds <- mapM getStoreTime ds
    let clean = all (< kt) ds
    when (not clean) $
        putStore k =<< act

makeDirtyBit :: Eq v => Build Applicative (Changed k v, ()) k v
makeDirtyBit = withChangedApplicative $ topological $ \k ds act -> do
    dirty <- getChanged k ||^ anyM getChanged ds
    when dirty $
        putStore k =<< act


type MakeHash k v = Map.Map (k, [Hash v]) (Hash v)


makeTrace :: Hashable v => Build Applicative (MakeHash k v) k v
makeTrace = topological $ \k ds act -> do
    now <- getStoreHash k
    ds <- mapM getStoreHash ds
    res <- Map.lookup (k, ds) <$> getInfo
    when (Just now /= res) $ do
        res <- act
        modifyInfo $ Map.insert (k, ds) (getHash res)
        putStore k res


shakeDirtyBit :: Eq v => Build Monad (Changed k v, ()) k v
shakeDirtyBit = withChangedMonad $ recursive $ \k ds ask act -> do
    dirty <- getChanged k ||^ maybe (return True) (anyM (\x -> ask x >> getChanged x)) ds
    when dirty $
        putStore k . snd =<< act


-- | The simplified Shake approach of recording previous traces
shake :: Hashable v => Build Monad [Trace k v] k v
shake = recursive $ \k _ ask act -> do
    poss <- traces (\k v -> (==) v . getHash <$> ask k) k =<< getInfo
    v <- getStoreHash k
    when (v `notElem` poss) $ do
        (ds, v) <- act
        putStore k v
        dsh <- mapM getStoreHash ds
        modifyInfo (Trace k (zip ds dsh) (getHash v) :)


spreadsheetTrace :: (Hashable v) => Build Monad ([Trace k v], [k]) k v
spreadsheetTrace = reordering $ \k ds ask act -> do
    poss <- traces (\k v -> (== Just v) . fmap getHash <$> ask k) k . fst =<< getInfo
    v <- getStoreHash k
    if v `elem` poss then
        return Nothing
    else do
        res <- act
        case res of
            Left e -> return $ Just e
            Right (ds, v) -> do
                putStore k v
                dsh <- mapM getStoreHash ds
                modifyInfo $ first (Trace k (zip ds dsh) (getHash v) :)
                return Nothing


spreadsheet :: Eq v => Build Monad (Changed k v, [k]) k v
spreadsheet = withChangedMonad $ reordering $ \k ds _ act -> do
    dirty <- getChanged k ||^ maybe (return True) (anyM getChanged) ds
    if not dirty then
        return Nothing
    else do
        res <- act
        case res of
            Left e -> return $ Just e
            Right (_, v) -> do
                putStore k v
                return Nothing

data TraceContent k v = TraceContent
    {tcTraces :: [Trace k v]
    ,tcContent :: Map.Map (Hash v) v
    } deriving Show

instance Default (TraceContent k v) where def = TraceContent def def

bazel :: Hashable v => Build Applicative (TraceContent k v) k v
bazel = topological $ \k ds act -> do
    poss <- traces (\k v -> (== v) <$> getStoreHash k) k . tcTraces =<< getInfo
    if null poss then do
        res <- act
        dsh <- mapM getStoreHash ds
        modifyInfo $ \i -> i
            {tcTraces = Trace k (zip ds dsh) (getHash res) : tcTraces i
            ,tcContent = Map.insert (getHash res) res $ tcContent i}
        putStore k res
    else do
        now <- getStoreHash k
        when (now `notElem` poss) $
            putStore k . (Map.! head poss) . tcContent =<< getInfo


shazel :: Hashable v => Build Monad (TraceContent k v) k v
shazel = recursive $ \k _ ask act -> do
    poss <- traces (\k v -> (== v) . getHash <$> ask k) k . tcTraces =<< getInfo
    if null poss then do
        (ds, v) <- act
        dsh <- mapM getStoreHash ds
        modifyInfo $ \i -> i
            {tcTraces = Trace k (zip ds dsh) (getHash v) : tcTraces i
            ,tcContent = Map.insert (getHash v) v $ tcContent i}
        putStore k v
    else do
        now <- getStoreHash k
        when (now `notElem` poss) $
            putStore k . (Map.! head poss) . tcContent =<< getInfo


spreadsheetRemote :: Hashable v => Build Monad (TraceContent k v, [k]) k v
spreadsheetRemote = reordering $ \k _ ask act -> do
    poss <- traces (\k v -> (== Just v) . fmap getHash <$> ask k) k . tcTraces . fst =<< getInfo
    if null poss then do
        res <- act
        case res of
            Left e -> return $ Just e
            Right (ds, v) -> do
                dsv <- mapM getStoreHash ds
                modifyInfo $ first $ \i -> i
                    {tcTraces = Trace k (zip ds dsv) (getHash v) : tcTraces i
                    ,tcContent = Map.insert (getHash v) v $ tcContent i}
                putStore k v
                return Nothing
    else do
        now <- getStoreHash k
        when (now `notElem` poss) $
            putStore k . (Map.! head poss) . tcContent . fst =<< getInfo
        return Nothing
