{-# LANGUAGE ConstraintKinds, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, KindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Build.Task.Typed (TTask, Key (..), digits, fetch) where

import Data.Functor.Const
import Data.Functor.Identity

class KeyC k where
  type Value k :: *
  showKey :: k -> String

type TTask c k = forall f. c f => (forall k. KeyC k => k -> f (Value k)) -> k -> Maybe (f (Value k))

data Key a where
    Base       :: Key Int
    Number     :: Key Int
    SplitDigit :: Key (Int, Int)
    LastDigit  :: Key Int
    BaseDigits :: Key [Int]

instance Show (Key t) where
    show key = case key of
        Base       -> "Base"
        Number     -> "Number"
        SplitDigit -> "SplitDigit"
        LastDigit  -> "LastDigit"
        BaseDigits -> "BaseDigits"

instance KeyC (Key a) where
  type Value (Key a) = a
  showKey = show

digits :: TTask Applicative (Key a)
digits fetch SplitDigit = Just $ divMod <$> fetch Number <*> fetch Base
digits fetch LastDigit  = Just $ snd <$> fetch SplitDigit
digits fetch BaseDigits = Just $ enumFromTo 1 <$> fetch Base
digits _ _ = Nothing

data SomeKey where
  MkSomeKey :: KeyC k => k -> SomeKey

dependencies :: TTask Applicative k -> k -> [SomeKey]
dependencies task = maybe [] getConst . task (\k -> Const [MkSomeKey k])

dependencies_useful :: TTask Applicative k -> k -> [String]
dependencies_useful task k = map showSomeKey (dependencies task k) where
  showSomeKey :: SomeKey -> String
  showSomeKey (MkSomeKey k) = showKey k

fetch :: Key t -> Identity (Value (Key t))
fetch key = Identity $ case key of
    Base       -> 10
    Number     -> 2018
    SplitDigit -> (201, 8)
    LastDigit  -> 8
    BaseDigits -> [0..9]
