{-# LANGUAGE DeriveFunctor #-}
module Development.Build.Example.Expression where

import Control.Monad

import Development.Build.Compute
import Development.Build.Store

-- | Expression keys include:
-- * Variables with 'String' names.
-- * Simple functions with statically known dependencies, such as 'Add'.
-- * Functions that require dynamic dependencies, such as 'Ackermann'.
data Key = Variable String
         | Add Key Key
         | Ackermann Integer Integer
         deriving (Eq, Ord, Show)

-- | Teh 'Value' datatype includes information about possible failures.
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
type ExpressionStore = Store Key (Value Integer)

-- | We use 'mapStore' defined in "Development.Build.Store", using 'KeyNotFound'
-- as a default 'Value' constructor in case a key is missing.
expressionStore :: ExpressionStore
expressionStore = mapStore KeyNotFound

-- | Computation of expressions.
compute :: Compute Key (Value Integer)
compute store key = case key of
    Add k1 k2 -> return (liftM2 (+) (getValue store k1) (getValue store k2), [k1, k2])

    Ackermann m n -> return result
      where
        result | m < 0 || n < 0 = (ComputeError (show key ++ " is not defined"), [])
               | m == 0         = (Value (n + 1), [])
               | n == 0         = let x = Ackermann (m - 1) 0 in (getValue store x, [x])
               | otherwise      = let x = Ackermann m (n - 1) in case getValue store x of
                   Value index -> let y = Ackermann (m - 1) index
                                  in (getValue store y, [x, y])
                   failure     -> (failure, [])

    -- All other keys correspond to inputs
    _ -> defaultCompute store key
