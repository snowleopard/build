{-# LANGUAGE RankNTypes #-}
module Build.Task.Functor (dependency) where

import Data.Functor.Const

import Build.Task

dependency :: Task Functor k v -> k
dependency task = getConst $ task Const
