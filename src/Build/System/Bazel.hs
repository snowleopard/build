module Build.System.Bazel (bazel) where

import Control.Monad.State

import Build
import Build.Algorithm
import Build.Store
import Build.Trace

bazel :: (Ord k, Hashable v) => Build Applicative (CT k v) k v
bazel = topological $ \key deps act -> do
    ct <- gets getInfo
    store <- get
    dirty <- not <$> verifyCT (\k -> return $ getHash k store) key ct
    when dirty $ do
        maybeValue <- constructCT (\k -> return $ getHash k store) key ct
        case maybeValue of
            Just value -> modify (putValue key value)
            Nothing -> do
                value <- act
                modify $ \s ->
                    let newS = putValue key value s
                    in updateInfo (recordCT newS key deps) newS
