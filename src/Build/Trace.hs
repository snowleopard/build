module Build.Trace (Trace (..), traceMatch, Traces (..)) where

import Build.Store

import Control.Monad.Extra
import Data.Map (Map)

data Trace k v = Trace
    { key          :: k
    , depends :: [(k, Hash v)]
    , result       :: Hash v }

-- Determine whether a trace is relevant to the current state
traceMatch :: (Monad m, Eq k) => (k -> Hash v -> m Bool) -> k -> [Trace k v] -> m [Hash v]
traceMatch check key ts = mapMaybeM f ts
    where f (Trace k dkv v) = do
                b <- return (key == k) &&^ allM (uncurry check) dkv
                return $ if b then Just v else Nothing

data Traces k v = Traces
    { traces :: [Trace k v]
    , contents  :: Map (Hash v) v }
