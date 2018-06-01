{-# LANGUAGE ConstraintKinds, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, KindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | A Typed version of dependencies where the value type depends on the key.
module Build.Task.Typed (TTask, Key (..), digits, fetch) where

import Data.Functor.Const
import Data.Functor.Identity

class Key k where
    type Value k :: *
    showKey :: k -> String

type TTask c k = forall f. c f => (forall k. Key k => k -> f (Value k)) -> k -> Maybe (f (Value k))

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

digits :: TTask Applicative (ExampleKey a)
digits fetch SplitDigit = Just $ divMod <$> fetch Number <*> fetch Base
digits fetch LastDigit  = Just $ snd <$> fetch SplitDigit
digits fetch BaseDigits = Just $ enumFromTo 1 <$> fetch Base
digits _ _ = Nothing

data SomeKey where
    MkSomeKey :: Key k => k -> SomeKey

dependencies :: TTask Applicative k -> k -> [SomeKey]
dependencies task = maybe [] getConst . task (\k -> Const [MkSomeKey k])

showDependencies :: TTask Applicative k -> k -> [String]
showDependencies task k = map showSomeKey (dependencies task k)
  where
    showSomeKey :: SomeKey -> String
    showSomeKey (MkSomeKey k) = showKey k

fetch :: ExampleKey t -> Identity (Value (ExampleKey t))
fetch key = Identity $ case key of
    Base       -> 10
    Number     -> 2018
    SplitDigit -> (201, 8)
    LastDigit  -> 8
    BaseDigits -> [0..9]
