{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
module Development.Build.Example.Expression where

import Control.Monad

import Development.Build.Compute
import Development.Build.NonDeterministic
import Development.Build.Store

-- TODO: Add separate type for input keys.
-- | Expression keys include:
-- * Variables with 'String' names.
-- * Simple functions with statically known dependencies, such as 'Add'.
-- * Functions that require dynamic dependencies, such as 'Ackermann'.
data Key = Variable String
         | Add Key Key
         | Ackermann Integer Integer
         deriving (Eq, Ord, Show)

-- | The 'Value' datatype includes information about possible failures.
data Value a = Value a
             | KeyNotFound Key
             | ComputeError String
             deriving (Eq, Functor, Ord, Show)

instance Applicative Value where
    pure  = Value
    (<*>) = ap

instance Monad Value where
    return  = pure
    v >>= f = case v of Value a          -> f a
                        KeyNotFound  key -> KeyNotFound key
                        ComputeError msg -> ComputeError msg

-- | A key-value store for expressions.
type ExpressionStore = MapStore Key (Value Integer)

-- | We use 'mapStore' defined in "Development.Build.Store", using 'KeyNotFound'
-- as a default 'Value' constructor in case a key is missing.
-- expressionStore :: ExpressionStore m => m ()
-- expressionStore = mapStore KeyNotFound

-- runExpressionStore :: ExpressionStore a -> [(Key, Value Integer)] -> (a, )

-- | Computation of expressions.
compute :: Store m Key (Value Integer) => Compute m Key (Value Integer)
compute key = case key of
    Add k1 k2 -> do
        v1 <- getValue k1
        v2 <- getValue k2
        return $ deterministic (liftM2 (+) v1 v2)

    Ackermann m n -> result
      where
        result | m < 0 || n < 0 = return $ deterministic (ComputeError (show key ++ " is not defined"))
               | m == 0         = return $ deterministic (Value (n + 1))
               | n == 0         = deterministic <$> getValue (Ackermann (m - 1) 0)
               | otherwise      = do v <- getValue (Ackermann m (n - 1))
                                     case v of
                                        Value i -> deterministic <$> getValue (Ackermann (m - 1) i)
                                        failure -> return (deterministic failure)

    -- All other keys correspond to inputs
    Variable _ -> computeInput key
