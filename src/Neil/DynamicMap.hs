{-# LANGUAGE Rank2Types, FlexibleContexts, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

module Neil.DynamicMap(
    DynamicMap, empty, lookup, insert
    ) where

import Prelude hiding (lookup)
import Data.Dynamic
import Data.Typeable
import qualified Data.Map as Map


newtype DynamicMap = DynamicMap (Map.Map TypeRep Dynamic)
    deriving (Monoid, Show)


empty :: DynamicMap
empty = DynamicMap Map.empty

insert :: Typeable a => a -> DynamicMap -> DynamicMap
insert x (DynamicMap mp) = DynamicMap $ Map.insert (typeOf x) (toDyn x) mp

lookup :: forall a . Typeable a => DynamicMap -> Maybe a
lookup (DynamicMap mp) = flip fromDyn (error "can't happen") <$> Map.lookup (typeOf (undefined :: a)) mp
