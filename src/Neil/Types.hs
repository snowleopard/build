
module Neil.Types where

import Control.Monad.Trans.State
import qualified Data.Set as Set


data Disk i k v = Disk
    {diskInfo :: i
    ,diskFiles :: k -> Maybe v
    }

newtype Time = Time Double
    deriving (Eq,Ord,Show)

class HasTime v where getTime :: v -> Time

newtype Hash v = Hash Int
    deriving (Eq,Ord,Show)

class HasHash v where getHash :: v -> Hash v

instance HasHash Int where getHash = Hash

runM_ :: State ((), Disk i k v) a -> Disk i k v -> Disk i k v
runM_ = runM ()

runM :: t -> State (t, Disk i k v) a -> Disk i k v -> Disk i k v
runM t m d = snd $ execState m (t,d)

putFile :: Eq k => k -> v -> State (t, Disk i k v) v
putFile k v = do
    modify $ \(t, d) -> (t, d{diskFiles = \x -> if x == k then Just v else diskFiles d x})
    return v

getFile :: Show k => k -> State (t, Disk i k v) v
getFile k = do
    v <- getFileMaybe k
    maybe (fail $ "Couldn't find key " ++ show k) return v

getFileMaybe :: k -> State (t, Disk i k v) (Maybe v)
getFileMaybe k = do
    (_, d) <- get
    return $ diskFiles d k

getInfo :: State (t, Disk i k v) i
getInfo = do
    (_, d) <- get
    return $ diskInfo d

putInfo :: i -> State (t, Disk i k v) ()
putInfo i = updateInfo $ const i

updateInfo :: (i -> i) -> State (t, Disk i k v) ()
updateInfo f = modify $ \(t, d) -> (t, d{diskInfo = f $ diskInfo d})

getTemp :: State (t, Disk i k v) t
getTemp = gets fst

putTemp :: t -> State (t, Disk i k v) ()
putTemp t = updateTemp $ const t

updateTemp :: (t -> t) -> State (t, Disk i k v) ()
updateTemp f = modify $ \(t,x) -> (f t, x)


once :: (Show k, Ord k) => k -> State (Set.Set k, Disk i k v) v -> State (Set.Set k, Disk i k v) v
once k build = do
    done <- getTemp
    if k `Set.member` done then
        getFile k
    else do
        r <- build
        updateTemp $ Set.insert k
        return r
