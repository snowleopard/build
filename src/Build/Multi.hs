-- | Support for multiple-output tasks.
module Build.Multi (Partition, multi) where

import Data.Maybe
import Build.Task

-- | Defines a set partition. For a function to be a valid partition,
--   if @f k == ks@, then:
--
-- * @k \in ks@
--
-- * @forall i \in ks . f i == ks@
type Partition k = k -> [k]

-- | Given a task description with individual multiple-output keys, compute its
-- "closure" supporting all possible combinations of keys.
multi :: Eq k => Partition k -> Tasks Applicative [k] [v] -> Tasks Applicative [k] [v]
multi partition tasks keys
    | k:_ <- keys, partition k == keys = tasks keys
    | otherwise = Just $ Task $ \fetch ->
        sequenceA [ select k <$> fetch (partition k) | k <- keys ]
  where
    select k = fromMaybe (error msg) . lookup k . zip (partition k)
    msg = "Partition invariants violated"
