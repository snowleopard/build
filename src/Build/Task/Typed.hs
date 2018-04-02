{-# LANGUAGE ConstraintKinds, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, KindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Build.Task.Typed (TTask, T (..), Key (..), digits, dependencies, fetch) where

import Data.Functor.Const
import Data.Functor.Identity

type family Value k :: *

type TTask c k = forall f (t :: T). c f => (forall (t :: T). k t -> f (Value (k t))) -> k t -> Maybe (f (Value (k t)))

data T = S | P | L deriving (Eq, Show)

data Key t where
    Base       :: Key S
    Number     :: Key S
    SplitDigit :: Key P
    LastDigit  :: Key S
    BaseDigits :: Key L

instance Show (Key t) where
    show key = case key of
        Base       -> "Base"
        Number     -> "Number"
        SplitDigit -> "SplitDigit"
        LastDigit  -> "LastDigit"
        BaseDigits -> "BaseDigits"

type instance Value (Key S) = Integer
type instance Value (Key P) = (Integer, Integer)
type instance Value (Key L) = [Integer]

digits :: TTask Applicative Key
digits fetch SplitDigit = Just $ divMod <$> fetch Number <*> fetch Base
digits fetch LastDigit  = Just $ snd <$> fetch SplitDigit
digits fetch BaseDigits = Just $ enumFromTo 1 <$> fetch Base
digits _ _ = Nothing

dependencies :: TTask Applicative Key -> Key t -> [String]
dependencies task = maybe [] getConst . task (\k -> Const [show k])

fetch :: Key t -> Identity (Value (Key t))
fetch key = Identity $ case key of
    Base       -> 10
    Number     -> 2018
    SplitDigit -> (201, 8)
    LastDigit  -> 8
    BaseDigits -> [0..9]
