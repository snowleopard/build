{-# LANGUAGE CPP, ConstraintKinds, RankNTypes, GADTs, TypeFamilies #-}
#if __GLASGOW_HASKELL__ < 800
{-# OPTIONS_GHC -Wno-unused-binds #-}
#else
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
#endif
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | A model of polymorphic tasks, where the value type depends on the key.
-- See the source for an example.
module Build.Task.Typed (Task, dependencies) where

#if __GLASGOW_HASKELL__ < 800
import Control.Applicative
#else
import Data.Functor.Const
#endif

-- | The @fetch@ callback whose result type depends on the type of the key.
type Fetch k f = forall a. k a -> f a

-- | A typed build task.
type Task c k = forall f a. c f => Fetch k f -> k a -> Maybe (f a)

-- | A way to show the name of a key.
type ShowKey k = forall a. k a -> String

-- | Extract the names of dependencies.
dependencies :: ShowKey k -> Task Applicative k -> k a -> [String]
dependencies showKey task = maybe [] getConst . task (\k -> Const [showKey k])

------------------------------------ Example -----------------------------------
data ExampleKey a where
    Base       :: ExampleKey Int
    Number     :: ExampleKey Int
    SplitDigit :: ExampleKey (Int, Int)
    LastDigit  :: ExampleKey Int
    BaseDigits :: ExampleKey [Int]

showExampleKey :: ShowKey ExampleKey
showExampleKey key = case key of
    Base       -> "Base"
    Number     -> "Number"
    SplitDigit -> "SplitDigit"
    LastDigit  -> "LastDigit"
    BaseDigits -> "BaseDigits"

digits :: Task Applicative ExampleKey
digits fetch SplitDigit = Just $ divMod <$> fetch Number <*> fetch Base
digits fetch LastDigit  = Just $ snd <$> fetch SplitDigit
digits fetch BaseDigits = Just $ enumFromTo 1 <$> fetch Base
digits _ _ = Nothing

fetch :: Applicative f => ExampleKey a -> f a
fetch key = pure $ case key of
    Base       -> 10
    Number     -> 2018
    SplitDigit -> (201, 8)
    LastDigit  -> 8
    BaseDigits -> [0..9]
