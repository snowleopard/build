{-# LANGUAGE RankNTypes #-}
module Build.Task.Functor (inputTask, dependency) where

import Data.Functor.Const

import Build.Task

dependency :: Task Functor k v -> k -> Maybe k
dependency task = fmap getConst . task Const
