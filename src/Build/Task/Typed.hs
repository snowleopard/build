{-# LANGUAGE ConstraintKinds, RankNTypes, GADTs, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | A model of polymorphic tasks, where the value type depends on the key.
-- See the source for an example.
module Build.Task.Typed (Task, Key (..), showDependencies) where

import Data.Functor.Const
import Data.Functor.Identity

-- | A type class for keys, equipped with an associated type family that
-- can be used to determine the type of value corresponding to the key.
class Key k where
    type Value k :: *
    -- | The name of the key. Useful for avoiding heterogeneous lists of keys.
    showKey :: k -> String

-- | A typed build task.
type Task c k = forall f. c f => (forall k. Key k => k -> f (Value k)) -> k -> Maybe (f (Value k))

-- | Extract the names of dependencies.
showDependencies :: Task Applicative k -> k -> [String]
showDependencies task = maybe [] getConst . task (\k -> Const [showKey k])

------------------------------------ Example -----------------------------------
data ExampleKey a where
    Base       :: ExampleKey Int
    Number     :: ExampleKey Int
    SplitDigit :: ExampleKey (Int, Int)
    LastDigit  :: ExampleKey Int
    BaseDigits :: ExampleKey [Int]

instance Show (ExampleKey a) where
    show key = case key of
        Base       -> "Base"
        Number     -> "Number"
        SplitDigit -> "SplitDigit"
        LastDigit  -> "LastDigit"
        BaseDigits -> "BaseDigits"

instance Key (ExampleKey a) where
    type Value (ExampleKey a) = a
    showKey = show

digits :: Task Applicative (ExampleKey a)
digits fetch SplitDigit = Just $ divMod <$> fetch Number <*> fetch Base
digits fetch LastDigit  = Just $ snd <$> fetch SplitDigit
digits fetch BaseDigits = Just $ enumFromTo 1 <$> fetch Base
digits _ _ = Nothing

fetch :: ExampleKey t -> Identity (Value (ExampleKey t))
fetch key = Identity $ case key of
    Base       -> 10
    Number     -> 2018
    SplitDigit -> (201, 8)
    LastDigit  -> 8
    BaseDigits -> [0..9]
