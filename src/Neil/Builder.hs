{-# LANGUAGE Rank2Types, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards #-}

module Neil.Builder(
    dumb,
    dumbOnce,
    make,
    shake,
    spreadsheet,
    bazel
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

newtype Once k = Once (Set.Set k)
    deriving Default

-- | Build a rule at most once in a single execution
once :: (Show k, Ord k, Typeable k) => k -> M k v i v -> M k v i v
once k build = do
    Once done <- getTemp
    if k `Set.member` done then
        getStore k
    else do
        r <- build
        modifyTemp $ \(Once set) -> Once $ Set.insert k set
        return r


-- | Figure out when files change, like a modtime
newtype StoreTime k v = StoreTime {fromStoreTime :: Map.Map k v}
    deriving Default

data Time = LastBuild | AfterLastBuild deriving (Eq,Ord)

getStoreTimeMaybe :: (Ord k, Eq v) => k -> M k v (StoreTime k v) (Maybe Time)
getStoreTimeMaybe k = do
    old <- Map.lookup k . fromStoreTime <$> getInfo
    new <- getStoreMaybe k
    return $ if isNothing new then Nothing else Just $ if old == new then LastBuild else AfterLastBuild

getStoreTime :: (Show k, Ord k, Eq v) => k -> M k v (StoreTime k v) Time
getStoreTime k = fromMaybe (error $ "no store time available for " ++ show k) <$> getStoreTimeMaybe k

returnStoreTime :: M k v (StoreTime k v) ()
returnStoreTime = putInfo . StoreTime =<< getStoreMap

-- | Take the transitive closure of a function
transitiveClosure :: Ord k => (k -> [k]) -> [k] -> [k]
transitiveClosure deps = f Set.empty
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
dumb :: Build Monad k v ()
dumb compute = runM . mapM_ f
    where f k = maybe (getStore k) (putStore k) =<< compute f k

-- | Refinement of dumb, compute everything but at most once per execution
dumbOnce :: Build Monad k v ()
dumbOnce compute = runM . mapM_ f
    where f k = once k $ maybe (getStore k) (putStore k) =<< compute f k


-- | The simplified Make approach where we build a dependency graph and topological sort it
make :: Eq v => Build Applicative k v (StoreTime k v)
make compute ks = runM $ do
    let depends = getDependencies compute
    forM_ (topSort depends $ transitiveClosure depends ks) $ \k -> do
        kt <- getStoreTimeMaybe k
        ds <- mapM getStoreTime $ depends k
        case kt of
            Just xt | all (<= xt) ds -> return ()
            _ -> maybe (return ()) (putStore_ k) =<< compute getStore k
    returnStoreTime


-- During the last execution, these were the traces I saw
type Shake k v = Map.Map k (Hash v, [(k, Hash v)])

-- | The simplified Shake approach of recording previous traces
shake :: Hashable v => Build Monad k v (Shake k v)
shake compute = runM . mapM_ f
    where
        f k = once k $ do
            info <- getInfo
            valid <- case Map.lookup k info of
                Nothing -> return False
                Just (me, deps) ->
                    (maybe False (== me) <$> getStoreHashMaybe k) &&^
                    allM (\(d,h) -> (== h) . getHash <$> f d) deps
            if valid then
                getStore k
            else do
                (ds, v) <- trackDependencies compute f k
                v <- maybe (getStore k) (putStore k) v
                dsh <- mapM (fmap getHash . getStore) ds
                modifyInfo $ Map.insert k (getHash v, zip ds dsh)
                return v


data Spreadsheet k v = Spreadsheet
    {ssOrder :: [k]
    ,ssPrevious :: Map.Map k v
    }

instance Default (Spreadsheet k v) where def = Spreadsheet def def

spreadsheet :: Eq v => Build Monad k v (Spreadsheet k v)
spreadsheet compute ks = runM $ do
    Spreadsheet{..} <- getInfo
    -- use explicit cleanliness rather than dirtiness so newly discovered items are dirty
    let isSame k = (==) (Map.lookup k ssPrevious) <$> getStoreMaybe k
    clean <- Set.fromList <$> filterM isSame ssOrder
    order <- f clean Set.empty $ ssOrder ++ (ks \\ ssOrder)
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
                res <- failDependencies compute f' k
                case res of
                    Left e -> f clean done ([e | e `notElem` ks] ++ ks ++ [k])
                    Right v -> do
                        maybe (return ()) (putStore_ k) v
                        (k :) <$> f (Set.delete k clean) (Set.insert k done) ks


data Bazel k v = Bazel
    {bzKnown :: Map.Map (k, [Hash v]) (Hash v)
    ,bzContent :: Map.Map (Hash v) v
    }

instance Default (Bazel k v) where def = Bazel def def

bazel :: Hashable v => Build Applicative k v (Bazel k v)
bazel compute ks = runM $ do
    let depends = getDependencies compute
    forM_ (topSort depends $ transitiveClosure depends ks) $ \k -> do
        ds <- mapM getStoreHash $ depends k
        res <- Map.lookup (k, ds) . bzKnown <$> getInfo
        case res of
            Nothing -> do
                res <- compute getStore k
                case res of
                    Nothing -> return ()
                    Just res -> do
                        modifyInfo $ \i -> i
                            {bzKnown = Map.insert (k, ds) (getHash res) $ bzKnown i
                            ,bzContent = Map.insert (getHash res) res $ bzContent i}
                        putStore_ k res
            Just res -> do
                now <- getStoreHashMaybe k
                when (now /= Just res) $
                    putStore_ k . (Map.! res) . bzContent =<< getInfo
