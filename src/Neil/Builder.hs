{-# LANGUAGE Rank2Types, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards, ConstraintKinds #-}

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
-- DEPENDENCY ORDER SCHEMES

linear :: Default i => (k -> [k] -> M i k v v -> M i k v ()) -> Build Applicative i k v
linear step compute k = runM $ do
    let depends = getDependencies compute
    forM_ (topSort depends $ transitiveClosure depends k) $ \k ->
        case compute getStore k of
            Nothing -> return ()
            Just act -> step k (depends k) act
    where
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


newtype Recursive k = Recursive (Set.Set k)
    deriving Default

-- | Build a rule at most once in a single execution
recursive :: Default i => (k -> (k -> M i k v v) -> M i k v ([k], v) -> M i k v ()) -> Build Monad i k v
recursive step compute = runM . ensure
    where
        ensure k = do
            let ask x = ensure x >> getStore x
            Recursive done <- getTemp
            when (k `Set.notMember` done) $ do
                modifyTemp $ \(Recursive set) -> Recursive $ Set.insert k set
                case trackDependencies compute ask k of
                    Nothing -> return ()
                    Just act -> step k ask act


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

returnStoreTime :: Build Applicative (StoreTime k v) k v -> Build Applicative (StoreTime k v) k v
returnStoreTime op compute k i mp = let (_,mp2) = op compute k i mp in (StoreTime mp, mp)



---------------------------------------------------------------------
-- BUILD SYSTEMS


-- | Dumbest build system possible, always compute everything from scratch multiple times
dumb :: Build Monad () k v
dumb compute = runM . f
    where f k = maybe (getStore k) (putStore k =<<) $ compute f k

-- | Refinement of dumb, compute everything but at most once per execution
dumbOnce :: Build Monad () k v
dumbOnce = recursive $ \k _ act -> putStore_ k . snd =<< act


-- | The simplified Make approach where we build a dependency graph and topological sort it
make :: Eq v => Build Applicative (StoreTime k v) k v
make = returnStoreTime $ linear $ \k ds act -> do
        kt <- getStoreTimeMaybe k
        ds <- mapM getStoreTime ds
        case kt of
            Just xt | all (<= xt) ds -> return ()
            _ -> putStore_ k =<< act

type MakeHash k v = Map.Map (k, [Hash v]) (Hash v)


makeHash :: Hashable v => Build Applicative (MakeHash k v) k v
makeHash = linear $ \k ds act -> do
    now <- getStoreHashMaybe k
    ds <- mapM getStoreHash ds
    res <- Map.lookup (k, ds) <$> getInfo
    when (isNothing now || now /= res) $ do
        res <- act
        modifyInfo $ Map.insert (k, ds) (getHash res)
        putStore_ k res


-- During the last execution, these were the traces I saw
type Shake k v = Map.Map k (Hash v, [(k, Hash v)])

-- | The simplified Shake approach of recording previous traces
shake :: Hashable v => Build Monad (Shake k v) k v
shake = recursive $ \k ask act -> do
    info <- getInfo
    valid <- case Map.lookup k info of
        Nothing -> return False
        Just (me, deps) ->
            (maybe False (== me) <$> getStoreHashMaybe k) &&^
            allM (\(d,h) -> (== h) . getHash <$> ask d) deps
    unless valid $ do
        (ds, v) <- act
        putStore k v
        dsh <- mapM (fmap getHash . getStore) ds
        modifyInfo $ Map.insert k (getHash v, zip ds dsh)


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
bazel = linear $ \k ds act -> do
    ds <- mapM getStoreHash ds
    res <- Map.lookup (k, ds) . bzKnown <$> getInfo
    case res of
        Nothing -> do
            res <- act
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
shazel = recursive $ \k ask act -> do
    poss <- Map.findWithDefault [] k . szKnown <$> getInfo
    res <- flip filterM poss $ \(ShazelResult ds r) -> allM (\(k,h) -> (==) h . getHash <$> ask k) ds
    case res of
        [] -> do
            (ds, v) <- act
            dsv <- mapM getStoreHash ds
            modifyInfo $ \i -> i
                {szKnown = Map.insertWith (++) k [ShazelResult (zip ds dsv) (getHash v)] $ szKnown i
                ,szContent = Map.insert (getHash v) v $ szContent i}
            putStore_ k v
        _ -> do
            let poss = [v | ShazelResult _ v <- res]
            now <- getStoreHashMaybe k
            when (now `notElem` map Just poss) $
                putStore_ k . (Map.! head poss) . szContent =<< getInfo
