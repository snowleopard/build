module Build.System.Shake (shake, cloudShake) where

import Control.Monad.State

import Build
import Build.Algorithm
import Build.Store
import Build.Trace

import qualified Data.Map as Map

-- Shake build system
shake :: (Eq k, Hashable v) => Build Monad [Trace k v] k v
shake = recursive $ \key fetch act -> do
    traces <- gets (getInfo . fst)
    poss <- traceMatch (\k v -> (==) v . hash <$> fetch k) key traces
    current <- gets (getHash key . fst)
    when (current `notElem` poss) $ do
        (v, ds) <- act
        modify $ \(s, done) ->
            let t = Trace key [(d, getHash d s) | d <- ds] (getHash key s)
            in (putInfo (t : getInfo s) (putValue key v s), done)

cloudShake :: (Eq k, Hashable v) => Build Monad (Traces k v) k v
cloudShake = recursive $ \key fetch act -> do
    s <- gets fst
    let Traces traces contents = getInfo s
    poss <- traceMatch (\k v -> (==) v . hash <$> fetch k) key traces
    if null poss then do
        (v, ds) <- act
        modify $ \(s,done) ->
            let t = Trace key [(d, getHash d s) | d <- ds] (getHash key s)
                ts = Traces (t : traces) (Map.insert (hash v) v contents)
            in (putInfo ts (putValue key v s), done)
    else do
        s <- gets fst
        when (getHash key s `notElem` poss) $
            modify $ \(s, done) -> (putValue key (contents Map.! head poss) s, done)
