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
--
-- A side observation: we could also rewrite the type of `Task` into
--
-- type Task c k = forall f. c f => (forall a. k a -> f a) -> (forall a. k a -> Maybe (f a))
--
-- ...which looks like a morphism between natural transformations. I'll let
-- category theory enthusiasts explain what this strange creature is doing here.
type Task c k = forall f a. c f => Fetch k f -> k a -> Maybe (f a)

-- | A way to show the name of a key.
type ShowKey k = forall a. k a -> String

-- | Extract the names of dependencies.
dependencies :: ShowKey k -> Task Applicative k -> k a -> [String]
dependencies showKey task = maybe [] getConst . task (\k -> Const [showKey k])

------------------------------------ Example -----------------------------------
data Key a where
    Base       :: Key Int
    Number     :: Key Int
    SplitDigit :: Key (Int, Int)
    LastDigit  :: Key Int
    BaseDigits :: Key [Int]

-- | A build task for some simple typed numeric calculations. We can perform
-- static analysis of this task using the function 'dependencies'. For example:
--
-- @
-- dependencies showKey task Base       == []
-- dependencies showKey task SplitDigit == ["Number","Base"]
-- @
task :: Task Applicative Key
task fetch SplitDigit = Just $ divMod <$> fetch Number <*> fetch Base
task fetch LastDigit  = Just $ snd <$> fetch SplitDigit
task fetch BaseDigits = Just $ (\b -> [0..(b - 1)]) <$> fetch Base
task _ _ = Nothing

-- | An example key/value mapping consistent with the build 'task'.
fetch :: Applicative f => Fetch Key f
fetch key = pure $ case key of
    Base       -> 10
    Number     -> 2018
    SplitDigit -> (201, 8)
    LastDigit  -> 8
    BaseDigits -> [0..9]

-- | Show the name of a key.
showKey :: ShowKey Key
showKey key = case key of
    Base       -> "Base"
    Number     -> "Number"
    SplitDigit -> "SplitDigit"
    LastDigit  -> "LastDigit"
    BaseDigits -> "BaseDigits"
