{-# LANGUAGE ConstraintKinds, DeriveFunctor, FlexibleContexts #-}
module Development.Build.Example.Expression where

import Control.Monad

import Development.Build.Compute
import Development.Build.Store

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

functorialComputeExample :: Compute (Functor f, Get f Key (Value Integer)) f Key (Value Integer)
functorialComputeExample key = case key of
    Increment k -> increment <$> getValue k
    _ -> computeInput key

applicativeComputeExample :: Compute (Applicative f, Get f Key (Value Integer)) f Key (Value Integer)
applicativeComputeExample key = case key of
    Add k1 k2 -> add <$> getValue k1 <*> getValue k2
    _ -> computeInput key

monadicComputeExample :: Compute (Monad f, Get f Key (Value Integer)) f Key (Value Integer)
monadicComputeExample key = case key of
    Ackermann m n -> result
      where
        result | m < 0 || n < 0 = return $ ComputeError (show key ++ " is not defined")
               | m == 0         = return $ Value (n + 1)
               | n == 0         = getValue (Ackermann (m - 1) 0)
               | otherwise      = do v <- getValue (Ackermann m (n - 1))
                                     case v of
                                        Value i -> getValue (Ackermann (m - 1) i)
                                        failure -> return failure
    _ -> computeInput key

-- | Computation of expressions.
compute :: Compute (Monad f, Get f Key (Value Integer)) f Key (Value Integer)
compute key = case key of
    Increment _   -> functorialComputeExample  key
    Add _ _       -> applicativeComputeExample key
    Ackermann _ _ -> monadicComputeExample     key

    -- All other keys correspond to inputs
    Variable _ -> computeInput key
