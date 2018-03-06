{-# LANGUAGE DeriveFunctor, RankNTypes #-}
module Development.Build.NonDeterministic (NonDeterministic (..)) where

import Data.Functor.Identity
import Control.Monad
import Control.Monad.List
import System.Random

-- | A monad capable of running non-deterministic computations. Given a result
-- of an 'Alternative' of 'MonadPlus' compute, chooses either to fail or return
-- a random result. Note the parametericity guarantees that if a @Just value@ is
-- returned it is indeed a possible result.
class Monad m => NonDeterministic m where
    choose :: (forall f. MonadPlus f => f a) -> m (Maybe a)

instance NonDeterministic IO where
    choose x = do
        let possible = runIdentity (runListT x)
            results  = Nothing : map Just possible
        index <- randomRIO (1, length results)
        return $ results !! index
