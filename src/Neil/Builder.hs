{-# LANGUAGE Rank2Types, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards, ConstraintKinds, TypeFamilies, LambdaCase #-}

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
import Data.Either.Extra
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as Set hiding (Set)
import qualified Data.Map as Map hiding (Map)

---------------------------------------------------------------------
-- DEPENDENCY ORDER SCHEMES

topological
    :: (m ~ M i k v, Default i)
    => (k -> [k] -> m v -> m ())
    -> Build Applicative i k v
topological step compute k = runM $ do
    let depends = getDependencies compute
    forM_ (topSort depends $ transitiveClosure depends k) $ \k ->
        case compute getStore k of
            Nothing -> return ()
            Just act -> step k (depends k) act


newtype Recursive k = Recursive (Set k)
    deriving Default

-- | Build a rule at most once in a single execution
recursive
    :: (m ~ M i k v, Default i)
    => (k -> Maybe [k] -> (k -> m v) -> m ([k], v) -> m ())
    -> Build Monad i k v
recursive step compute = runM . ensure
    where
        ensure k = do
            let fetch x = ensure x >> getStore x
            Recursive done <- getTemp
            when (k `Set.notMember` done) $ do
                modifyTemp $ \(Recursive done) -> Recursive $ Set.insert k done
                case trackDependencies compute fetch k of
                    Nothing -> return ()
                    Just act -> step k (getDependenciesMaybe compute k) fetch act


reordering
    :: (m ~ M (i, [k]) k v, Default i)
    => (k -> Maybe [k] -> (k -> m (Maybe v)) -> m (Either k ([k], v)) -> m (Maybe k))
    -> Build Monad (i, [k]) k v
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
                    step k (getDependenciesMaybe compute k) (fmap eitherToMaybe . f') act >>= \case
                        Nothing -> (k :) <$> f (Set.insert k done) ks
                        Just e -> f done $ [e | e `notElem` ks] ++ ks ++ [k]


---------------------------------------------------------------------
-- DIRTY SCHEMES

data Trace k v = Trace
    {key :: k
    ,dependencies :: [(k, Hash v)]
    ,result :: Hash v
    } deriving Show

data Traces k v = Traces
    {traces :: [Trace k v]
    ,contents :: Map (Hash v) v
    } deriving Show

instance Default (Traces k v) where def = Traces def def

-- | Given a way to fetch for a dependency, and the key under construction, and the traces, pick the hashes that match
traceMatch :: (Monad m, Eq k) => (k -> Hash v -> m Bool) -> k -> [Trace k v] -> m [Hash v]
traceMatch check k ts = mapMaybeM f ts
    where f (Trace k2 dkv v) = do
                b <- return (k == k2) &&^ allM (uncurry check) dkv
                return $ if b then Just v else Nothing


---------------------------------------------------------------------
-- BUILD SYSTEMS


-- | Dumbest build system possible, always compute everything from scratch multiple times
dumb :: Build Monad () k v
dumb compute k = runM (f k)
    where
        f k = case compute f k of
            Nothing -> getStore k
            Just act -> do
                v <- act
                putStore k v
                return v

-- | Refinement of dumb, compute everything but at most once per execution
dumbRecursive :: Build Monad () k v
dumbRecursive = recursive $ \k _ _ act -> putStore k . snd =<< act

dumbTopological :: Build Applicative () k v
dumbTopological = topological $ \k _ act -> putStore k =<< act

dumbDynamic :: Build Monad ((), [k]) k v
dumbDynamic = reordering $ \k _ _ act -> do
    act >>= \case
        Left e -> return $ Just e
        Right (_, v) -> do
            putStore k v
            return Nothing


-- | The simplified Make approach where we build a dependency graph and topological sort it
make :: Eq v => Build Applicative (Changed k v, ()) k v
make = withChangedApplicative $ topological $ \k dk act -> do
    t <- getStoreTime k
    dt <- mapM getStoreTime dk
    let dirty = any (>= t) dt
    when dirty $
        putStore k =<< act

makeDirtyBit :: Eq v => Build Applicative (Changed k v, ()) k v
makeDirtyBit = withChangedApplicative $ topological $ \k dk act -> do
    dirty <- getChanged k ||^ anyM getChanged dk
    when dirty $
        putStore k =<< act


makeTrace :: Hashable v => Build Applicative [Trace k v] k v
makeTrace = topological $ \k dk act -> do
    poss <- traceMatch (\k v -> (==) v <$> getStoreHash k) k =<< getInfo
    h <- getStoreHash k
    when (h `notElem` poss) $ do
        v <- act
        dh <- mapM getStoreHash dk
        modifyInfo (Trace k (zip dk dh) (getHash v) :)
        putStore k v


shakeDirtyBit :: Eq v => Build Monad (Changed k v, ()) k v
shakeDirtyBit = withChangedMonad $ recursive $ \k dk fetch act -> do
    dirty <- getChanged k ||^ maybe (return True) (anyM (\k -> fetch k >> getChanged k)) dk
    when dirty $
        putStore k . snd =<< act


-- | The simplified Shake approach of recording previous traces
shake :: Hashable v => Build Monad [Trace k v] k v
shake = recursive $ \k _ fetch act -> do
    poss <- traceMatch (\k v -> (==) v . getHash <$> fetch k) k =<< getInfo
    h <- getStoreHash k
    when (h `notElem` poss) $ do
        (dk, v) <- act
        putStore k v
        dh <- mapM getStoreHash dk
        modifyInfo (Trace k (zip dk dh) (getHash v) :)


spreadsheetTrace :: (Hashable v) => Build Monad ([Trace k v], [k]) k v
spreadsheetTrace = reordering $ \k dk fetch act -> do
    poss <- traceMatch (\k v -> (== Just v) . fmap getHash <$> fetch k) k . fst =<< getInfo
    h <- getStoreHash k
    if h `elem` poss then
        return Nothing
    else
        act >>= \case
            Left e -> return $ Just e
            Right (dk, v) -> do
                putStore k v
                dh <- mapM getStoreHash dk
                modifyInfo $ first (Trace k (zip dk dh) (getHash v) :)
                return Nothing


spreadsheet :: Eq v => Build Monad (Changed k v, [k]) k v
spreadsheet = withChangedMonad $ reordering $ \k dk _ act -> do
    dirty <- getChanged k ||^ maybe (return True) (anyM getChanged) dk
    if not dirty then
        return Nothing
    else
        act >>= \case
            Left e -> return $ Just e
            Right (_, v) -> do
                putStore k v
                return Nothing

bazel :: Hashable v => Build Applicative (Traces k v) k v
bazel = topological $ \k dk act -> do
    poss <- traceMatch (\k v -> (== v) <$> getStoreHash k) k . traces =<< getInfo
    if null poss then do
        v <- act
        dh <- mapM getStoreHash dk
        modifyInfo $ \i -> i
            {traces = Trace k (zip dk dh) (getHash v) : traces i
            ,contents = Map.insert (getHash v) v $ contents i}
        putStore k v
    else do
        h <- getStoreHash k
        when (h `notElem` poss) $
            putStore k . (Map.! head poss) . contents =<< getInfo


shazel :: Hashable v => Build Monad (Traces k v) k v
shazel = recursive $ \k _ fetch act -> do
    poss <- traceMatch (\k v -> (== v) . getHash <$> fetch k) k . traces =<< getInfo
    if null poss then do
        (dk, v) <- act
        dh <- mapM getStoreHash dk
        modifyInfo $ \i -> i
            {traces = Trace k (zip dk dh) (getHash v) : traces i
            ,contents = Map.insert (getHash v) v $ contents i}
        putStore k v
    else do
        now <- getStoreHash k
        when (now `notElem` poss) $
            putStore k . (Map.! head poss) . contents =<< getInfo


spreadsheetRemote :: Hashable v => Build Monad (Traces k v, [k]) k v
spreadsheetRemote = reordering $ \k _ fetch act -> do
    poss <- traceMatch (\k v -> (== Just v) . fmap getHash <$> fetch k) k . traces . fst =<< getInfo
    if null poss then do
        res <- act
        case res of
            Left e -> return $ Just e
            Right (ds, v) -> do
                dsv <- mapM getStoreHash ds
                modifyInfo $ first $ \i -> i
                    {traces = Trace k (zip ds dsv) (getHash v) : traces i
                    ,contents = Map.insert (getHash v) v $ contents i}
                putStore k v
                return Nothing
    else do
        now <- getStoreHash k
        when (now `notElem` poss) $
            putStore k . (Map.! head poss) . contents . fst =<< getInfo
        return Nothing
