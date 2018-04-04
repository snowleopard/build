module Build.System.Bazel (bazel) where

import Control.Monad.State

import Build
import Build.Algorithm
import Build.Store
import Build.Trace

import qualified Data.Map as Map

bazel :: (Ord k, Hashable v) => Build Applicative (Traces k v) k v
bazel = topological $ \key ds act -> do
    s <- get
    let Traces traces contents = getInfo s
    poss <- traceMatch (\k v -> return $ getHash k s == v) key traces
    if null poss then do
        v <- act
        modify $ \s ->
            let t = Trace key [(d, getHash d s) | d <- ds] (getHash key s)
                ts = Traces (t : traces) (Map.insert (hash v) v contents)
            in putInfo ts (putValue key v s)
    else do
        when (getHash key s `notElem` poss) $
            modify $ putValue key (contents Map.! head poss)
