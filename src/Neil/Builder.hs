{-# LANGUAGE Rank2Types, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards #-}

module Neil.Builder(
    dumb,
    dumbOnce,
    make,
    makeHash,
    shake,
    spreadsheet,
    bazel,
    shazel
    ) where

import Neil.Build
import Neil.Compute
import Control.Monad.Extra
import Data.Default
import Data.Maybe
import Data.List
import Data.Typeable
import qualified Data.Set as Set
import qualified Data.Map as Map

---------------------------------------------------------------------
-- UTILITIES

newtype Recursive k = Recursive (Set.Set k)
    deriving Default

-- | Build a rule at most once in a single execution
recursive :: (Show k, Ord k, Typeable k) => k -> M i k v v -> M i k v v
recursive k build = do
    Recursive done <- getTemp
    if k `Set.member` done then
        getStore k
    else do
        r <- build
        modifyTemp $ \(Recursive set) -> Recursive $ Set.insert k set
        return r


-- | Figure out when files change, like a modtime
newtype StoreTime k v = StoreTime {fromStoreTime :: Map.Map k v}
    deriving (Default, Show)

data Time = LastBuild | AfterLastBuild deriving (Eq,Ord)

getStoreTimeMaybe :: (Ord k, Eq v) => k -> M (StoreTime k v) k v (Maybe Time)
getStoreTimeMaybe k = do
    old <- Map.lookup k . fromStoreTime <$> getInfo
    new <- getStoreMaybe k
    return $ if isNothing new then Nothing else Just $ if old == new then LastBuild else AfterLastBuild

getStoreTime :: (Show k, Ord k, Eq v) => k -> M (StoreTime k v) k v Time
getStoreTime k = fromMaybe (error $ "no store time available for " ++ show k) <$> getStoreTimeMaybe k

returnStoreTime :: M (StoreTime k v) k v ()
returnStoreTime = putInfo . StoreTime =<< getStoreMap

-- | Take the transitive closure of a function
transitiveClosure :: Ord k => (k -> [k]) -> k -> [k]
transitiveClosure deps k = f Set.empty [k]
    where
        f seen [] = Set.toList seen
        f seen (t:odo)
            | t `Set.member` seen = f seen odo
            | otherwise = f (Set.insert t seen) (deps t ++ odo)


-- | Topologically sort a list using the given dependency order
topSort :: Ord k => (k -> [k]) -> [k] -> [k]
topSort deps ks = f $ Map.fromList [(k, deps k) | k <- ks]
    where
        f mp
            | Map.null mp = []
            | Map.null leaf = error "cycles!"
            | otherwise = Map.keys leaf ++ f (Map.map (filter (`Map.notMember` leaf)) rest)
            where (leaf, rest) = Map.partition null mp


---------------------------------------------------------------------
-- BUILD SYSTEMS


-- | Dumbest build system possible, always compute everything from scratch multiple times
dumb :: Build Monad () k v
dumb compute = runM . f
    where f k = maybe (getStore k) (putStore k =<<) $ compute f k

-- | Refinement of dumb, compute everything but at most once per execution
dumbOnce :: Build Monad () k v
dumbOnce compute = runM . f
    where f k = recursive k $ maybe (getStore k) (putStore k =<<) $ compute f k


-- | The simplified Make approach where we build a dependency graph and topological sort it
make :: Eq v => Build Applicative (StoreTime k v) k v
make compute ks = runM $ do
    let depends = getDependencies compute
    forM_ (topSort depends $ transitiveClosure depends ks) $ \k -> do
        kt <- getStoreTimeMaybe k
        ds <- mapM getStoreTime $ depends k
        case kt of
            Just xt | all (<= xt) ds -> return ()
            _ -> maybe (return ()) (putStore_ k =<<) $ compute getStore k
    returnStoreTime

type MakeHash k v = Map.Map (k, [Hash v]) (Hash v)

makeHash :: Hashable v => Build Applicative (MakeHash k v) k v
makeHash compute ks = runM $ do
    let depends = getDependencies compute
    forM_ (topSort depends $ transitiveClosure depends ks) $ \k -> do
        now <- getStoreHashMaybe k
        ds <- mapM getStoreHash $ depends k
        res <- Map.lookup (k, ds) <$> getInfo
        when (isNothing now || now /= res) $ do
            case compute getStore k of
                Nothing -> return ()
                Just res -> do
                    res <- res
                    modifyInfo $ Map.insert (k, ds) (getHash res)
                    putStore_ k res


-- During the last execution, these were the traces I saw
type Shake k v = Map.Map k (Hash v, [(k, Hash v)])

-- | The simplified Shake approach of recording previous traces
shake :: Hashable v => Build Monad (Shake k v) k v
shake compute = runM . f
    where
        f k = recursive k $ do
            info <- getInfo
            valid <- case Map.lookup k info of
                Nothing -> return False
                Just (me, deps) ->
                    (maybe False (== me) <$> getStoreHashMaybe k) &&^
                    allM (\(d,h) -> (== h) . getHash <$> f d) deps
            case trackDependencies compute f k of
                Just res | not valid -> do
                    (ds, v) <- res
                    putStore k v
                    dsh <- mapM (fmap getHash . getStore) ds
                    modifyInfo $ Map.insert k (getHash v, zip ds dsh)
                    return v
                _ -> getStore k


data Spreadsheet k v = Spreadsheet
    {ssOrder :: [k]
    ,ssPrevious :: Map.Map k v
    } deriving Show

instance Default (Spreadsheet k v) where def = Spreadsheet def def

spreadsheet :: Eq v => Build Monad (Spreadsheet k v) k v
spreadsheet compute k = runM $ do
    Spreadsheet{..} <- getInfo
    -- use explicit cleanliness rather than dirtiness so newly discovered items are dirty
    let isSame k = (==) (Map.lookup k ssPrevious) <$> getStoreMaybe k
    clean <- Set.fromList <$> filterM isSame ssOrder
    order <- f clean Set.empty $ ssOrder ++ ([k] \\ ssOrder)
    putInfo . Spreadsheet order =<< getStoreMap
    where
        f clean done [] = return []
        f clean done (k:ks) = do
            let deps = getDependenciesMaybe compute k
            let isClean = k `Set.member` clean &&
                          maybe False (all (`Set.member` clean)) deps
            if isClean then do
                f clean (Set.insert k done) ks
            else do
                let f' x = if x `Set.member` done then Right <$> getStore x else return $ Left x
                case failDependencies compute f' k of
                    Nothing -> (k :) <$> f (Set.delete k clean) (Set.insert k done) ks
                    Just res -> do
                        res <- res
                        case res of
                            Left e -> f clean done ([e | e `notElem` ks] ++ ks ++ [k])
                            Right v -> do
                                putStore_ k v
                                (k :) <$> f (Set.delete k clean) (Set.insert k done) ks


data Bazel k v = Bazel
    {bzKnown :: Map.Map (k, [Hash v]) (Hash v)
    ,bzContent :: Map.Map (Hash v) v
    } deriving Show

instance Default (Bazel k v) where def = Bazel def def

bazel :: Hashable v => Build Applicative (Bazel k v) k v
bazel compute ks = runM $ do
    let depends = getDependencies compute
    forM_ (topSort depends $ transitiveClosure depends ks) $ \k -> do
        ds <- mapM getStoreHash $ depends k
        res <- Map.lookup (k, ds) . bzKnown <$> getInfo
        case res of
            Nothing -> do
                case compute getStore k of
                    Nothing -> return ()
                    Just res -> do
                        res <- res
                        modifyInfo $ \i -> i
                            {bzKnown = Map.insert (k, ds) (getHash res) $ bzKnown i
                            ,bzContent = Map.insert (getHash res) res $ bzContent i}
                        putStore_ k res
            Just res -> do
                now <- getStoreHashMaybe k
                when (now /= Just res) $
                    putStore_ k . (Map.! res) . bzContent =<< getInfo


data ShazelResult k v = ShazelResult [(k, Hash v)] (Hash v) deriving Show

data Shazel k v = Shazel
    {szKnown :: Map.Map k [ShazelResult k v]
    ,szContent :: Map.Map (Hash v) v
    } deriving Show

instance Default (Shazel k v) where def = Shazel def def

shazel :: Hashable v => Build Monad (Shazel k v) k v
shazel compute = runM . f
    where
        f k = recursive k $ do
            poss <- Map.findWithDefault [] k . szKnown <$> getInfo
            res <- flip filterM poss $ \(ShazelResult ds r) -> allM (\(k,h) -> (==) h . getHash <$> f k) ds
            case res of
                [] -> do
                    case trackDependencies compute f k of
                        Nothing -> getStore k
                        Just res -> do
                            (ds, v) <- res
                            dsv <- mapM getStoreHash ds
                            modifyInfo $ \i -> i
                                {szKnown = Map.insertWith (++) k [ShazelResult (zip ds dsv) (getHash v)] $ szKnown i
                                ,szContent = Map.insert (getHash v) v $ szContent i}
                            putStore k v
                _ -> do
                    let poss = [v | ShazelResult _ v <- res]
                    now <- getStoreHashMaybe k
                    if (now `elem` map Just poss) then
                        getStore k
                    else
                        putStore k . (Map.! head poss) . szContent =<< getInfo
