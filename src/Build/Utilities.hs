module Build.Utilities (
    -- * Graph operations
    graph, reachable, topSort, reach, reachM,

    -- * Transformers
    AltConst (..),

    -- * Logic combinators
    forall, forallM, exists, existsM, (==>)
    ) where

import Algebra.Graph hiding (graph)
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.Class        as C

import Control.Applicative
import Data.Functor.Identity
import qualified Data.Set as Set

-- | Build a dependency graph given a function for computing dependencies of a
-- key and a target key.
graph :: Ord k => (k -> [k]) -> k -> Graph k
graph deps key = transpose $ overlays [ star k (deps k) | k <- keys Set.empty [key] ]
  where
    keys seen []   = Set.toList seen
    keys seen (x:xs)
        | x `Set.member` seen = keys seen xs
        | otherwise           = keys (Set.insert x seen) (deps x ++ xs)

-- | Compute all keys reachable via dependecies from a target key.
reachable :: Ord k => (k -> [k]) -> k -> [k]
reachable deps key = vertexList (graph deps key)

topSort :: Ord k => Graph k -> Maybe [k]
topSort = AM.topSort . C.toGraph

-- TODO: Just switch to a proper graph library for reachability analysis.
-- | Given a function to compute successors of a vertex, apply it recursively
-- starting from a given vertex. Returns @Nothing@ if this process does not
-- terminate because of cycles. Note that the current implementation is very
-- inefficient: it trades efficiency for simplicity. The resulting list is
-- likely to contain an exponential number of duplicates.
reach :: Eq a => (a -> [a]) -> a -> Maybe [a]
reach successors = runIdentity . reachM (return . successors)

-- | Given a monadic function to compute successors of a vertex, apply it
-- recursively starting from a given vertex. Returns @Nothing@ if this process
-- does not terminate because of cycles. Note that the current implementation is
-- very inefficient: it trades efficiency for simplicity. The resulting list is
-- likely to contain an exponential number of duplicates.
reachM :: (Eq a, Monad m) => (a -> m [a]) -> a -> m (Maybe [a])
reachM successors a = fmap (filter (/= a)) <$> go [] a
  where
    go xs x | x `elem` xs = return Nothing -- A cycle is detected
            | otherwise   = do res <- traverse (go (x:xs)) =<< successors x
                               return $ ((x:xs)++) . concat <$> sequence res

-- TODO: We can probably achieve the same effect using Compose [] Const.
newtype AltConst a b = AltConst { getAltConst :: [[a]] }

instance Functor (AltConst a) where
    fmap _ (AltConst xs) = AltConst xs

instance Applicative (AltConst a) where
    pure _ = AltConst [[]]
    AltConst xs <*> AltConst ys = AltConst [ x ++ y | x <- xs, y <- ys ]

instance Alternative (AltConst a) where
    empty = AltConst []
    AltConst xs <|> AltConst ys = AltConst (xs ++ ys)

-- | Check that a predicate holds for all values of @a@.
forall :: (a -> Bool) -> Bool
forall = undefined

-- | Check that a monadic predicate holds for all values of @a@.
forallM :: (a -> m Bool) -> m Bool
forallM = undefined

-- | Check that a predicate holds for some value of @a@.
exists :: (a -> Bool) -> Bool
exists = undefined

-- | Check that a monadic predicate holds for some value of @a@.
existsM :: (a -> m Bool) -> m Bool
existsM = undefined

-- | Logical implication.
(==>) :: Bool -> Bool -> Bool
x ==> y = not x || y

infixr 0 ==>
