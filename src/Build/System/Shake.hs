module Build.System.Shake (shake, cloudShake) where

import Control.Monad.State

import Build
import Build.Algorithm
import Build.Store
import Build.Trace

-- Shake build system
shake :: (Eq k, Hashable v) => Build Monad (VT k v) k v
shake = recursive $ \key fetch act -> do
    vt <- gets (getInfo . fst)
    dirty <- not <$> verifyVT (fmap hash . fetch) key vt
    when dirty $ do
        (value, deps) <- act
        modify $ \(s, t) ->
            let newS = putValue key value s
            in (updateInfo (recordVT newS key deps) newS, t)

cloudShake :: (Eq k, Hashable v) => Build Monad (CT k v) k v
cloudShake = recursive $ \key fetch act -> do
    ct <- gets (getInfo . fst)
    dirty <- not <$> verifyCT (fmap hash . fetch) key ct
    when dirty $ do
        maybeValue <- constructCT (fmap hash . fetch) key ct
        case maybeValue of
            Just value -> modify $ \(s, t) -> (putValue key value s, t)
            Nothing -> do
                (value, deps) <- act
                modify $ \(s, t) ->
                    let newS = putValue key value s
                    in (updateInfo (recordCT newS key deps) newS, t)
