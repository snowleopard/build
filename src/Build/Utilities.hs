-- | General utilities useful in the rest of the package
module Build.Utilities (
    -- * Graph operations
    graph, reachable, topSort, reach, reachM,

    -- * Logic combinators
    forall
    ) where

import Algebra.Graph
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.Class        as C

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

-- | Compute the topological sort of a graph or return @Nothing@ if the graph
-- has cycles.
topSort :: Ord k => Graph k -> Maybe [k]
topSort = AM.topSort . C.toGraph

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

-- | Check that a predicate holds for all values of @a@.
forall :: (a -> Bool) -> Bool
forall = error "This combinator is used only for type checking"
