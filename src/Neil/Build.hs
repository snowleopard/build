{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Neil.Build(
    dumb,
    dumbOnce,
    make,
    Shake, shake,
    ) where

import Neil.Constraints
import Control.Monad.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map


-- | Dumbest build system possible, always compute everything from scratch multiple times
dumb :: (Build m k v) => [k] -> m ()
dumb = mapM_ f
    where f k = maybe (getStore k) (putStore k) =<< run f k


newtype Once k = Once (Set.Set k)

once :: (Build m k v, Temp m (Once k)) => k -> m v -> m v
once k build = do
    Once done <- getTemp
    if k `Set.member` done then
        getStore k
    else do
        r <- build
        updateTemp $ \(Once set) -> Once $ Set.insert k set
        return r


-- | Refinement of dumb, compute everything but at most once per execution
dumbOnce :: (Build m k v, Temp m (Once k)) => [k] -> m ()
dumbOnce = mapM_ f
    where f k = once k $ maybe (getStore k) (putStore k) =<< run f k


-- | The simplified Make approach where we build a dependency graph and topological sort it
make :: (Build m k v, PreDepends m k, HasTime v) => [k] -> m ()
make ks = do
    deps <- depends
    forM_ (topSort $ unroll deps ks) $ \k -> do
        kt <- fmap getTime <$> getStoreMaybe k
        ds <- mapM (fmap getTime . getStore) $ deps k
        case kt of
            Just xt | all (< xt) ds -> return ()
            _ -> maybe (return ()) (void . putStore k) =<< run getStore k

unroll :: Ord k => (k -> [k]) -> [k] -> Map.Map k [k]
unroll deps = f Map.empty
    where
        f mp [] = mp
        f mp (t:odo)
            | t `Map.member` mp = f mp odo
            | otherwise = let ds = deps t in f (Map.insert t ds mp) (ds ++ odo)

topSort :: Ord k => Map.Map k [k] -> [k]
topSort mp
    | Map.null mp = []
    | Map.null leaf = error "cycles!"
    | otherwise = Map.keys leaf ++ topSort (Map.map (filter (`Map.notMember` leaf)) rest)
    where (leaf, rest) = Map.partition null mp


-- During the last execution, these were the traces I saw
type Shake k v = Map.Map k (Hash v, [(k, Hash v)])

-- | The simplified Shake approach of recording previous traces
shake :: (Build m k v, Info m (Shake k v), Temp m (Once k), HasHash v) => [k] -> m ()
shake = mapM_ f
    where
        f k = once k $ do
            info <- getInfo
            valid <- case Map.lookup k info of
                Nothing -> return False
                Just (me, deps) ->
                    (maybe False ((==) me . getHash) <$> getStoreMaybe k) &&^
                    allM (\(d,h) -> (== h) . getHash <$> f d) deps
            if valid then
                getStore k
            else do
                (ds, v) <- runTrace f k
                v <- maybe (getStore k) (putStore k) v
                dsh <- mapM (fmap getHash . getStore) ds
                updateInfo $ Map.insert k (getHash v, zip ds dsh)
                return v
