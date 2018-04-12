{-# LANGUAGE Rank2Types, ConstraintKinds #-}
module Build.Multi(
    multi
    ) where

import Data.List
import Data.Maybe
import Build.Task


-- | Defines a set partition. For a function to be a valid partition,
--   if @f k == ks@, then:
--
-- * @k \in ks@
--
-- * @forall i \in ks . f i == ks@
type Partition k = k -> [k]

-- Given a build rule where you can build some combinations of multiple rules,
-- use a partition to enable building lots of multiple rule subsets.
multi :: Eq k => Partition k -> Tasks Applicative [k] [v] -> Tasks Applicative [k] [v]
multi part old ks
    | k:_ <- ks, part k == ks = old ks
    | otherwise = Just $ Task $ \fetch ->
        sequenceA [(!! index k) <$> fetch (part k) | k <- ks]
    where index k = fromJust $ elemIndex k $ part k
