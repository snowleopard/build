{-# LANGUAGE ConstraintKinds, DeriveFunctor, FlexibleContexts, Rank2Types #-}
module Development.Build.Example.Expression where

import Control.Applicative
import Control.Monad

import Development.Build.Compute

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

instance Alternative Value where
    empty   = ComputeError "Computation failure"
    x <|> y = case x of
        ComputeError _ -> y
        _              -> x

instance MonadPlus Value where
    mzero = empty
    mplus = (<|>)

functorialComputeExample :: FunctorialCompute Key (Value Integer)
functorialComputeExample getValue key = case key of
    Increment k -> Just . increment <$> getValue k
    _ -> undefined

applicativeComputeExample :: ApplicativeCompute Key (Value Integer)
applicativeComputeExample getValue key = case key of
    Add k1 k2 -> Just <$> (add <$> getValue k1 <*> getValue k2)
    _ -> pure Nothing

monadicComputeExample :: MonadicCompute Key (Value Integer)
monadicComputeExample getValue key = case key of
    Ackermann m n -> Just <$> result
      where
        result | m < 0 || n < 0 = return $ ComputeError (show key ++ " is not defined")
               | m == 0         = return $ Value (n + 1)
               | n == 0         = getValue (Ackermann (m - 1) 1)
               | otherwise      = do v <- getValue (Ackermann m (n - 1))
                                     case v of
                                        Value i -> getValue (Ackermann (m - 1) i)
                                        failure -> return failure
    _ -> return Nothing

-- | Computation of expressions.
compute :: MonadicCompute Key (Value Integer)
compute getValue key = case key of
    Increment _   -> functorialComputeExample  getValue key
    Add _ _       -> applicativeComputeExample getValue key
    Ackermann _ _ -> monadicComputeExample     getValue key

    -- All other keys correspond to inputs
    Variable _ -> inputCompute getValue key
