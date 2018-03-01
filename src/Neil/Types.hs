
module Neil.Types where

import Control.Monad.Trans.State
import qualified Data.Set as Set


data Disk i k v = Disk
    {diskInfo :: i
    ,diskFiles :: k -> Maybe v
    }

newtype Time = Time Double
    deriving (Eq,Ord)

class HasTime v where getTime :: v -> Time

newtype Hash v = Hash Int
    deriving (Eq,Ord)

class HasHash v where getHash :: v -> Hash v

runM_ :: State ((), Disk i k v) a -> Disk i k v -> Disk i k v
runM_ = runM ()

runM :: t -> State (t, Disk i k v) a -> Disk i k v -> Disk i k v
runM t m d = snd $ execState m (t,d)

putFile :: k -> v -> State (t, Disk i k v) v
putFile = undefined

getFile :: k -> State (t, Disk i k v) v
getFile = undefined

getFileMaybe :: k -> State (t, Disk i k v) (Maybe v)
getFileMaybe = undefined

getInfo :: State (t, Disk i k v) i
getInfo = undefined

putInfo :: i -> State (t, Disk i k v) ()
putInfo = undefined

updateInfo :: (i -> i) -> State (t, Disk i k v) ()
updateInfo = undefined

getTemp :: State (t, Disk i k v) t
getTemp = undefined

putTemp :: t -> State (t, Disk i k v) ()
putTemp = undefined

updateTemp :: (t -> t) -> State (t, Disk i k v) ()
updateTemp = undefined


once :: Ord k => k -> State (Set.Set k, Disk i k v) v -> State (Set.Set k, Disk i k v) v
once k build = do
    done <- getTemp
    if k `Set.member` done then
        getFile k
    else do
        r <- build
        updateTemp $ Set.insert k
        return r
