{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable, TupleSections #-}

-- | Build traces that are used for recording information from previuos builds.
module Build.Trace (
    Trace (..),

    -- * Verifying traces
    VT, recordVT, verifyVT,

    -- * Constructive traces
    CT, isDirtyCT, recordCT, constructCT,

    -- * Constructive traces optimised for deep tasks
    DCT, recordDCT, constructDCT,

    -- * Step traces
    Step, ST, recordST, verifyST
    ) where

import Build.Store

import Control.Monad.Extra
import Data.Maybe
import Data.List
import Data.Semigroup

-- | A trace is parameterised by the types of keys @k@, hashes @h@, as well as the
-- result @r@. For verifying traces, @r = h@; for constructive traces, @Hash r = h@.
data Trace k h r = Trace
    { key     :: k
    , depends :: [(k, h)]
    , result  :: r }
    deriving Show

------------------------------- Verifying traces -------------------------------

-- | An abstract data type for a set of verifying traces equipped with 'recordVT',
-- 'verifyVT' and a 'Monoid' instance.
newtype VT k v = VT [Trace k (Hash v) (Hash v)] deriving (Monoid, Semigroup, Show)

-- | Record a new trace for building a @key@ with dependencies @deps@, obtaining
-- the hashes of up-to-date values by using @fetchHash@.
recordVT :: Monad m => k -> Hash v -> [k] -> (k -> m (Hash v)) -> VT k v -> m (VT k v)
recordVT key valueHash deps fetchHash (VT ts) = do
    hs <- mapM fetchHash deps
    return $ VT $ Trace key (zip deps hs) valueHash : ts

-- | Given a function to compute the hash of a key's current value,
-- a @key@, and a set of verifying traces, return 'True' if the @key@ is
-- up-to-date.
verifyVT :: (Monad m, Eq k, Eq v) => k -> Hash v -> (k -> m (Hash v)) -> VT k v -> m Bool
verifyVT key valueHash fetchHash (VT ts) = anyM match ts
  where
    match (Trace k deps result)
        | k /= key || result /= valueHash = return False
        | otherwise = andM [ (h==) <$> fetchHash k | (k, h) <- deps ]

------------------------------ Constructive traces -----------------------------

-- | An abstract data type for a set of constructive traces equipped with
-- 'recordCT', 'isDirtyCT', 'constructCT' and a 'Monoid' instance.
newtype CT k v = CT [Trace k (Hash v) v] deriving (Monoid, Semigroup, Show)

-- | Check if a given @key@ is dirty w.r.t a @store@.
isDirtyCT :: (Eq k, Hashable v) => k -> Store (CT k v) k v -> Bool
isDirtyCT key store = let CT ts = getInfo store in not (any match ts)
  where
    match (Trace k deps result) = k == key
                               && result == getValue key store
                               && and [ getHash k store == h | (k, h) <- deps ]

-- | Record a new trace for building a @key@ with dependencies @deps@, obtaining
-- the hashes of up-to-date values by using @fetchHash@.
recordCT :: Monad m => k -> v -> [k] -> (k -> m (Hash v)) -> CT k v -> m (CT k v)
recordCT key value deps fetchHash (CT ts) = do
    hs <- mapM fetchHash deps
    return $ CT $ Trace key (zip deps hs) value : ts

-- | Given a function to compute the hash of a key's current value,
-- a @key@, and a set of constructive traces, return @Just newValue@ if it is
-- possible to reconstruct it from the traces. Prefer reconstructing the
-- currenct value, if it matches one of the traces.
constructCT :: (Monad m, Eq k, Eq v) => k -> (k -> m (Hash v)) -> CT k v -> m [v]
constructCT key fetchHash (CT ts) = catMaybes <$> mapM match ts
  where
    match (Trace k deps result)
        | k /= key  = return Nothing
        | otherwise = do
            sameInputs <- andM [ (h==) <$> fetchHash k | (k, h) <- deps ]
            return $ if sameInputs then Just result else Nothing

--------------------------- Deep constructive traces ---------------------------

-- | Our current model has the same representation as 'CT', but requires an
-- additional invariant: if a DCT contains a trace for a key @k@, then it must
-- also contain traces for each of its non-input dependencies.
newtype DCT k v = DCT [Trace k (Hash v) v] deriving (Monoid, Semigroup, Show)

-- | Extract the tree of input dependencies of a given key.
deepDependencies :: (Eq k, Hashable v) => DCT k v -> Hash v -> k -> [k]
deepDependencies (DCT ts) valueHash key =
    case [ map fst deps | Trace k deps v <- ts, k == key, hash v == valueHash ] of
        []       -> [key] -- The @key@ is an input
        (deps:_) -> deps  -- We assume there is only one record for a pair (k, v)

-- | Record a new trace for building a @key@ with dependencies @deps@, obtaining
-- the hashes of up-to-date values from the given @store@.
recordDCT :: forall k v m. (Eq k, Hashable v, Monad m)
          => k -> v -> [k] -> (k -> m (Hash v)) -> DCT k v -> m (DCT k v)
recordDCT key value deps fetchHash dct@(DCT ts) = do
    let deepDeps = concatMap (deepDependencies dct $ hash value) deps
    hs <- mapM fetchHash deepDeps
    return $ DCT $ Trace key (zip deepDeps hs) value : ts

-- | Given a function to compute the hash of a key's current value,
-- a @key@, and a set of deep constructive traces, return
-- @Just newValue@ if it is possible to reconstruct it from the traces.
constructDCT :: forall k v m. (Eq k, Hashable v, Monad m)
             => k -> (k -> m (Hash v)) -> DCT k v -> m [v]
constructDCT key fetchHash (DCT ts) = constructCT key fetchHash (CT ts)

----------------- Step traces: a refinement of verifying traces ----------------
-- Step traces are an optimised version of the direct implementation of
-- verifying traces (as given by the 'VT' datatype), which is used by Shake.
-- They support the same high-level interface that allows to verify if a key is
-- up to date ('verifyST') as well as record new traces ('recordST').

newtype Step = Step Int deriving (Enum, Eq, Ord, Show)
instance Semigroup Step where Step a <> Step b = Step $ a + b
instance Monoid Step where mempty = Step 0; mappend = (<>)

-- | A step trace, records the resulting value, the step it last build, the step
-- where it changed.
newtype ST k v = ST [Trace k () (Hash v, Step, Step)]
    deriving (Monoid, Semigroup, Show)

latestST :: Eq k => k -> ST k v -> Maybe (Trace k () (Hash v, Step, Step))
latestST k (ST ts) = fmap snd $ listToMaybe $ reverse $ sortOn fst
    [(step, t) | t@(Trace k2 _ (_, step, _)) <- ts, k == k2]

-- | Record a new trace for building a @key@ with dependencies @deps@.
recordST :: (Hashable v, Eq k, Monad m) => Step -> k -> v -> [k] -> ST k v -> m (ST k v)
recordST step key value deps (ST ts) = do
    let hv = hash value
    let lastChange = case latestST key (ST ts) of
            -- I rebuilt, didn't change, so use the old change time
            Just (Trace _ _ (hv2, _, chng)) | hv2 == hv -> chng
            _ -> step
    return $ ST $ Trace key (map (,()) deps) (hash value, step, lastChange) : ts

-- | Given a function to compute the hash of a key's current value,
-- a @key@, and a set of verifying traces, return 'True' if the @key@ is
-- up-to-date.
verifyST :: (Monad m, Eq k, Hashable v) => k -> v -> (k -> m ()) -> m (ST k v) -> m Bool
verifyST key value demand st = do
    me <- latestST key <$> st
    case me of
        Just (Trace _ deps (hv, built, _)) | hash value == hv -> do
            mapM_ (demand . fst) deps
            st <- st
            -- things with no traces must be inputs, which I'm going to ignore for now...
            return $ and [ built >= chng | Just (Trace _ _ (_, _, chng)) <- map (flip latestST st . fst) deps]
        _ -> return False
