{-# LANGUAGE ConstraintKinds, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, KindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Build.Task.Typed (TTask, Key (..), digits, fetch) where

import Data.Functor.Const
import Data.Functor.Identity

type family Value k :: *

type TTask c k = forall f. c f => (forall k. k -> f (Value k)) -> k -> Maybe (f (Value k))

data Key a where
    Base       :: Key Int
    Number     :: Key Int
    SplitDigit :: Key (Int, Int)
    LastDigit  :: Key Int
    BaseDigits :: Key [Int]

type instance Value (Key a) = a

digits :: TTask Applicative (Key a)
digits fetch SplitDigit = Just $ divMod <$> fetch Number <*> fetch Base
digits fetch LastDigit  = Just $ snd <$> fetch SplitDigit
digits fetch BaseDigits = Just $ enumFromTo 1 <$> fetch Base
digits _ _ = Nothing

-- dependencies :: TTask Applicative (Key a) -> Key a -> [Key a]
-- dependencies task = maybe [] getConst . task (\k -> Const [k])

fetch :: Key t -> Identity (Value (Key t))
fetch key = Identity $ case key of
    Base       -> 10
    Number     -> 2018
    SplitDigit -> (201, 8)
    LastDigit  -> 8
    BaseDigits -> [0..9]
