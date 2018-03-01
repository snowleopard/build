{-# LANGUAGE Rank2Types #-}

module Neil.Implementation(
    dumb,
    dumbOnce,
    make,
    Shake, shake,
    ) where

import Neil.Compute
import Neil.Types
import Control.Monad.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map


-- | Dumbest build system possible, always compute everything from scratch multiple times
dumb :: Compute Monad k v -> [k] -> Disk i k v -> Disk i k v
dumb comp = runM_ . mapM_ f
    where f k = maybe (getFile k) (putFile k) =<< comp f k


-- | Refinement of dumb, compute everything but at most once per execution
dumbOnce :: Ord k => Compute Monad k v -> [k] -> Disk i k v -> Disk i k v
dumbOnce comp = runM Set.empty . mapM_ f
    where
        f k = once k $
            maybe (getFile k) (putFile k) =<< comp f k


make :: (Ord k, HasTime v) => Compute Applicative k v -> [k] -> Disk i k v -> Disk i k v
make comp ks = runM_ (mapM_ f $ topSort graph)
    where
        graph = unroll (getDependencies comp) ks

        f k = do
            kt <- fmap getTime <$> getFileMaybe k
            ds <- mapM (fmap getTime . getFile) $ graph Map.! k
            case kt of
                Just xt | all (< xt) ds -> return ()
                _ -> maybe (return ()) (void . putFile k) =<< comp getFile k

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

shake :: (Ord k, HasHash v) => Compute Monad k v -> [k] -> Disk (Shake k v) k v -> Disk (Shake k v) k v
shake comp = runM Set.empty . mapM_ f
    where
        f k = once k $ do
            info <- getInfo
            valid <- case Map.lookup k info of
                Just (me, deps) ->
                    (maybe False ((==) me . getHash) <$> getFileMaybe k) &&^
                    allM (\(d,h) -> (== h) . getHash <$> f d) deps
            if valid then
                getFile k
            else do
                (ds, v) <- trackDependencies comp f k
                v <- maybe (getFile k) (putFile k) v
                dsh <- mapM (fmap getHash . getFile) ds
                updateInfo $ Map.insert k (getHash v, zip ds dsh)
                return v
