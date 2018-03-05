{-# LANGUAGE RankNTypes #-}
module Development.Build.Compute.Alternative (
    failingCompute, (|||), dependencies, transitiveDependencies, acyclic
    ) where

import Control.Applicative
import Data.Maybe

import Development.Build.Compute
import Development.Build.Utilities

-- | The compute that fails for any key by returning 'empty'.
failingCompute :: Compute Alternative k v
failingCompute _ _ = empty

-- | Run the first compute then the second compute, combining the results.
(|||) :: Compute Alternative k v -> Compute Alternative k v -> Compute Alternative k v
(|||) compute1 compute2 get key = compute1 get key <|> compute2 get key

dependencies :: Compute Alternative k v -> k -> [[k]]
dependencies compute = getAltConst . compute (\k -> AltConst [[k]])

transitiveDependencies :: Eq k => Compute Alternative k v -> k -> [Maybe [k]]
transitiveDependencies compute = reachM (dependencies compute)

acyclic :: Eq k => Compute Alternative k v -> k -> Bool
acyclic compute = all isJust . transitiveDependencies compute

-- Probably not needed
-- data Script k v a where
--     Get  :: k -> Script k v v
--     Pure :: a -> Script k v a
--     Ap   :: Script k v (a -> b) -> Script k v a -> Script k v b
--     Zero :: Script k v a
--     Plus :: Script k v a -> Script k v a -> Script k v a

-- instance Functor (Script k v) where
--     fmap = Ap . Pure

-- instance Applicative (Script k v) where
--     pure  = Pure
--     (<*>) = Ap

-- instance Alternative (Script k v) where
--     empty = Zero
--     (<|>) = Plus

-- getScript :: Compute Alternative k v -> k -> Script k v (Maybe v)
-- getScript compute = compute Get
