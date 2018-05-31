{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable, TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Build.Trace (
    -- * Verifying traces
    VT, recordVT, verifyVT,

    -- * Step traces
    Step, ST, recordST, verifyST,

    -- * Constructive traces
    CT, isDirtyCT, recordCT, constructCT,

    -- * Constructive traces optimised for deterministic tasks
    DCT, recordDCT, constructDCT
    ) where

import Build.Store

import Control.Monad.Extra
import Data.Maybe
import Data.List
import Data.Semigroup

-- A trace is parameterised by the types of keys @k@, hashes @h@, as well as the
-- result @r@. For verifying traces, @r = h@; for constructive traces, @Hash r = h@.
data Trace k h r = Trace
    { key     :: k
    , depends :: [(k, h)]
    , result  :: r }
    deriving Show

-- | An abstract data type for a set of verifying traces equipped with 'record',
-- 'verify' and a 'Monoid' instance.
newtype VT k v = VT [Trace k (Hash v) (Hash v)] deriving (Monoid, Semigroup)

-- | Record a new trace for building a @key@ with dependencies @deps@, obtaining
-- the hashes of up-to-date values from the given @store@.
recordVT :: (Hashable v, Monad m) => k -> v -> [k] -> (k -> m (Hash v)) -> VT k v -> m (VT k v)
recordVT key value deps fetchHash (VT ts) = do
    hs <- mapM fetchHash deps
    return $ VT $ Trace key (zip deps hs) (hash value) : ts

-- | Given a function to compute the hash of a key's current value,
-- a @key@, and a set of verifying traces, return 'True' if the @key@ is
-- up-to-date.
verifyVT :: (Monad m, Eq k, Hashable v) => k -> v -> (k -> m (Hash v)) -> VT k v -> m Bool
verifyVT key value fetchHash (VT ts) = anyM match ts
  where
    match (Trace k deps result)
        | k /= key || result /= hash value = return False
        | otherwise = andM [ (h==) <$> fetchHash k | (k, h) <- deps ]

newtype Step = Step Int deriving (Enum,Eq,Ord,Show)
instance Semigroup Step where Step a <> Step b = Step $ a + b
instance Monoid Step where mempty = Step 0; mappend = (<>)

-- | A step trace, records the resulting value, the step it last build, the step where it changed
newtype ST k v = ST [Trace k () (Hash v, Step, Step)] deriving (Monoid, Semigroup,Show)

latestST :: Eq k => k -> ST k v -> Maybe (Trace k () (Hash v, Step, Step))
latestST k (ST ts) = fmap snd $ listToMaybe $ reverse $ sortOn fst [(step, t) | t@(Trace k2 _ (_, step, _)) <- ts, k == k2]

-- | Record a new trace for building a @key@ with dependencies @deps@, obtaining
-- the hashes of up-to-date values from the given @store@.
recordST :: (Hashable v, Eq k, Monad m) => Step -> k -> v -> [k] -> ST k v -> m (ST k v)
recordST step key value deps (ST ts) = do
    let hv = hash value
    let lastChange = case latestST key (ST ts) of
            Just (Trace _ _ (hv2, _, chng)) | hv2 == hv -> chng -- I rebuilt, didn't change, so use the old change time
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

newtype CT k v = CT [Trace k (Hash v) v] deriving (Monoid, Semigroup, Show)

isDirtyCT :: (Eq k, Hashable v) => k -> Store (CT k v) k v -> Bool
isDirtyCT key store = let CT ts = getInfo store in not (any match ts)
  where
    match (Trace k deps result) = k == key
                               && result == getValue key store
                               && and [ getHash k store == h | (k, h) <- deps ]

recordCT :: Monad m => k -> v -> [k] -> (k -> m (Hash v)) -> CT k v -> m (CT k v)
recordCT key value deps fetchHash (CT ts) = do
    hs <- mapM fetchHash deps
    return $ CT $ Trace key (zip deps hs) value : ts

-- Prefer constructing the currenct value, if it matches one of the traces.
constructCT :: (Monad m, Eq k, Eq v) => k -> v -> (k -> m (Hash v)) -> CT k v -> m (Maybe v)
constructCT key value fetchHash (CT ts) = do
    candidates <- catMaybes <$> mapM match ts
    if value `elem` candidates then return $ Just value
                               else return $ listToMaybe candidates
  where
    match (Trace k deps result)
        | k /= key  = return Nothing
        | otherwise = do
            sameInputs <- andM [ (h==) <$> fetchHash k | (k, h) <- deps ]
            return $ if sameInputs then Just result else Nothing

-- TODO: Dependencies actually form acyclic graphs, not trees. Trees are fine
-- from the correctness standpoint, but can be very inefficient if the graphs of
-- dependencies contain a lot of sharing. Switch to using graphs?
data Tree a = Leaf a | Node [Tree a]
    deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Hashable a => Hashable (Tree a) where
    hash (Leaf x) = Leaf <$> hash x
    hash (Node x) = Node <$> hash x

-- Invariant: if a DCT contains a trace for a key @k@, then it must also contain
-- traces for each of its non-input dependencies. Input keys cannot appear in a
-- DCT because they are never built.
newtype DCT k v = DCT [Trace k (Hash (Tree (Hash v))) v] deriving (Monoid, Semigroup)

-- Extract the tree of input dependencies of a given key.
inputTree :: Eq k => DCT k v -> k -> Tree k
inputTree dct@(DCT ts) key = case [ deps | Trace k deps _ <- ts, k == key ] of
    [] -> Leaf key
    deps:_ -> Node $ map (inputTree dct . fst) deps

-- Like 'inputTree', but replaces each key with the hash of its current value.
inputHashTree :: (Eq k, Monad m) => DCT k v -> (k -> m (Hash v)) -> k -> m (Tree (Hash v))
inputHashTree dct fetchHash = traverse fetchHash . inputTree dct

recordDCT :: forall k v m. (Hashable k, Hashable v, Monad m)
          => k -> v -> [k] -> (k -> m (Hash v)) -> DCT k v -> m (DCT k v)
recordDCT key value deps fetchHash (DCT ts) = do
    hs <- mapM depHash deps
    return $ DCT $ Trace key (zip deps hs) value : ts
  where
    depHash :: k -> m (Hash (Tree (Hash v)))
    depHash depKey = case [ deps | Trace k deps _ <- ts, k == depKey ] of
        [] -> hash . Leaf <$> fetchHash depKey -- depKey is an input
        deps:_ -> return $ fmap Node $ sequenceA $ map snd deps

constructDCT :: forall k v m. (Hashable k, Hashable v, Monad m)
             => k -> (k -> m (Hash v)) -> DCT k v -> m (Maybe v)
constructDCT key fetchHash dct@(DCT ts) = do
    candidates <- catMaybes <$> mapM match ts
    case candidates of
        []  -> return Nothing
        [v] -> return (Just v)
        _   -> error "Non-determinism detected"
  where
    match :: Trace k (Hash (Tree (Hash v))) v -> m (Maybe v)
    match (Trace k deps result)
        | k /= key  = return Nothing
        | otherwise = do
            sameInputs <- andM [ ((h ==) . hash) <$> inputHashTree dct fetchHash k | (k, h) <- deps ]
            return $ if sameInputs then Just result else Nothing
