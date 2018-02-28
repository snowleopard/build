{-# LANGUAGE ConstraintKinds, DeriveFunctor, FlexibleContexts, Rank2Types #-}
module Development.Build.Example.Expression where

import Control.Monad

import Development.Build.Compute.Applicative
import Development.Build.Compute.Functor
import Development.Build.Compute.Identity
import Development.Build.Compute.Monad

-- TODO: Good example is cyclic dependencies.
-- TODO: Add separate type for input keys.
-- TODO: Separate keys from formulas.
-- | Expression keys include:
-- * Variables with 'String' names.
-- * Simple functions with statically known dependencies, such as 'Add'.
-- * Functions that require dynamic dependencies, such as 'Ackermann'.
data Key = Variable String           -- No dependencies
         | Increment Key             -- Functorial dependencies
         | Add Key Key               -- Applicative dependencies
         | Ackermann Integer Integer -- Monadic dependencies
         deriving (Eq, Ord, Show)

-- | The 'Value' datatype includes information about possible failures.
data Value a = Value a
             | KeyNotFound Key
             | ComputeError String
             deriving (Eq, Functor, Ord, Show)

increment :: Value Integer -> Value Integer
increment value = fmap (+1) value

add :: Value Integer -> Value Integer -> Value Integer
add valueX valueY = do x <- valueX
                       y <- valueY
                       return (x + y)

instance Applicative Value where
    pure  = Value
    (<*>) = ap

instance Monad Value where
    return  = pure
    v >>= f = case v of Value a          -> f a
                        KeyNotFound  key -> KeyNotFound key
                        ComputeError msg -> ComputeError msg

-- | A key-value store for expressions.
-- type ExpressionStore = MapStore Key (Value Integer)

-- | We use 'mapStore' defined in "Development.Build.Store", using 'KeyNotFound'
-- as a default 'Value' constructor in case a key is missing.
-- expressionStore :: ExpressionStore m => m ()
-- expressionStore = mapStore KeyNotFound

-- runExpressionStore :: ExpressionStore a -> [(Key, Value Integer)] -> (a, )

functorialComputeExample :: FunctorialCompute Key (Value Integer)
functorialComputeExample getValue key = case key of
    Increment k -> increment <$> getValue k
    _ -> identityCompute getValue key

applicativeComputeExample :: ApplicativeCompute Key (Value Integer)
applicativeComputeExample getValue key = case key of
    Add k1 k2 -> add <$> getValue k1 <*> getValue k2
    _ -> identityCompute getValue key

monadicComputeExample :: MonadicCompute Key (Value Integer)
monadicComputeExample getValue key = case key of
    Ackermann m n -> result
      where
        result | m < 0 || n < 0 = return $ ComputeError (show key ++ " is not defined")
               | m == 0         = return $ Value (n + 1)
               | n == 0         = getValue (Ackermann (m - 1) 1)
               | otherwise      = do v <- getValue (Ackermann m (n - 1))
                                     case v of
                                        Value i -> getValue (Ackermann (m - 1) i)
                                        failure -> return failure
    _ -> identityCompute getValue key

-- | Computation of expressions.
compute :: MonadicCompute Key (Value Integer)
compute getValue key = case key of
    Increment _   -> functorialComputeExample  getValue key
    Add _ _       -> applicativeComputeExample getValue key
    Ackermann _ _ -> monadicComputeExample     getValue key

    -- All other keys correspond to inputs
    Variable _ -> identityCompute getValue key
