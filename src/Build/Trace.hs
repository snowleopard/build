{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable, TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Build.Trace (
    -- * Verifying traces
    VT, recordVT, verifyVT,

    -- * Constructive traces
    CT, recordCT, constructCT,

    -- * Constructive traces optimised for deterministic tasks
    DCT, recordDCT, constructDCT
    ) where

import Build.Store

import Control.Monad.Extra
import Data.Maybe
import Data.Semigroup

-- A trace is parameterised by the types of keys @k@, hashes @h@, as well as the
-- result @r@. For verifying traces, @r = h@; for constructive traces, @Hash r = h@.
data Trace k h r = Trace
    { key     :: k
    , depends :: [(k, h)]
    , result  :: r }

-- | An abstract data type for a set of verifying traces equipped with 'record',
-- 'verify' and a 'Monoid' instance.
newtype VT k v = VT [Trace k (Hash v) (Hash v)] deriving (Monoid, Semigroup)

-- | Record a new trace for building a @key@ with dependencies @deps@, obtaining
-- the hashes of up-to-date values from the given @store@.
recordVT :: (Hashable v, Monad m) => k -> v -> [k] -> (k -> m (Hash v)) -> m (VT k v)
recordVT key value deps fetchHash = do
    hs <- mapM fetchHash deps
    return $ VT [ Trace key (zip deps hs) (hash value) ]

-- | Given a function to compute the hash of a key's current value,
-- a @key@, and a set of verifying traces, return 'True' if the @key@ is
-- up-to-date.
verifyVT :: (Monad m, Eq k, Hashable v) => k -> v -> (k -> m (Hash v)) -> VT k v -> m Bool
verifyVT key value fetchHash (VT ts) = anyM match ts
  where
    match (Trace k deps result)
        | k /= key || result /= hash value = return False
        | otherwise = andM [ (h==) <$> fetchHash k | (k, h) <- deps ]

newtype CT k v = CT [Trace k (Hash v) v] deriving (Monoid, Semigroup)

recordCT :: Monad m => k -> v -> [k] -> (k -> m (Hash v)) -> m (CT k v)
recordCT key value deps fetchHash = do
    hs <- mapM fetchHash deps
    return $ CT ([Trace key (zip deps hs) value])

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
