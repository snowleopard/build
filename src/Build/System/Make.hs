{-# LANGUAGE ScopedTypeVariables #-}
module Build.System.Make (Time, MakeInfo, make) where

import Control.Monad.State

import Build
import Build.Algorithm
import Build.Store

type Time = Integer
type MakeInfo k = (k -> Time, Time)

make :: forall k v. Ord k => Build Applicative (MakeInfo k) k v
make = topological process
  where
    process :: k -> [k] -> State (Store (MakeInfo k) k v) v -> State (Store (MakeInfo k) k v) ()
    process key deps act = do
        (modTime, now) <- gets getInfo
        let dirty = or [ modTime dep > modTime key | dep <- deps ]
        when dirty $ do
            v <- act
            let newModTime k = if k == key then now else modTime k
            modify $ putInfo (newModTime, now + 1) . putValue key v
