{-# LANGUAGE RankNTypes #-}

-- | Given a build system that can work with single keys, generalise that to one
-- that deals with multiple keys at a time.
module Build.Multi (multi) where

import Data.Maybe
import Build.Task

-- | Defines a set partition. For a function to be a valid partition,
--   if @f k == ks@, then:
--
-- * @k \in ks@
--
-- * @forall i \in ks . f i == ks@
type Partition k = k -> [k]

-- | Given a build rule where you can build some combinations of multiple rules,
-- use a partition to enable building lots of multiple rule subsets.
multi :: Eq k => Partition k -> Tasks Applicative [k] [v] -> Tasks Applicative [k] [v]
multi partition tasks keys
    | k:_ <- keys, partition k == keys = tasks keys
    | otherwise = Just $ \fetch ->
        sequenceA [ select k <$> fetch (partition k) | k <- keys ]
  where
    select k = fromMaybe (error msg) . lookup k . zip (partition k)
    msg = "Partition invariants violated"
