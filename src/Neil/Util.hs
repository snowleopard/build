
module Neil.Util(
    transitiveClosure,
    topSort
    ) where

import qualified Data.Set as Set
import qualified Data.Map as Map


-- | Take the transitive closure of a function
transitiveClosure :: Ord k => (k -> [k]) -> k -> [k]
transitiveClosure deps k = f Set.empty [k]
    where
        f seen [] = Set.toList seen
        f seen (t:odo)
            | t `Set.member` seen = f seen odo
            | otherwise = f (Set.insert t seen) (deps t ++ odo)

-- | Topologically sort a list using the given dependency order
topSort :: Ord k => (k -> [k]) -> [k] -> [k]
topSort deps ks = f $ Map.fromList [(k, deps k) | k <- ks]
    where
        f mp
            | Map.null mp = []
            | Map.null leaf = error "cycles!"
            | otherwise = Map.keys leaf ++ f (Map.map (filter (`Map.notMember` leaf)) rest)
            where (leaf, rest) = Map.partition null mp
