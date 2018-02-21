module Development.Build.Example.Expression where

import Development.Build.Compute
import Development.Build.Store

-- | Expression keys include:
-- * Variables with 'String' names.
-- * Simple functions with statically known dependencies, such as 'Add'.
-- * Functions that require dynamic dependencies, such as 'Ackermann'.
-- * Configuration parameters, such as 'AckermannNegative1', which determines the
--   behaviour of the 'Ackermann' function when the first argument is negative.
data Key = Variable String
         | Add Key Key
         | Ackermann Integer Integer
         | AckermannNegative1
         | AckermannNegative2
         deriving (Eq, Ord, Show)

-- | We only consider integer values.
type Value = Integer

-- | Expression store.
type ExpressionStore = Store Key Value

-- | We use 'mapStore' defined in "Development.Build.Store".
expressionStore :: ExpressionStore
expressionStore = mapStore

-- | Computation of expressions.
compute :: Compute Key Value
compute store key = case key of
    Add k1 k2 -> return (getValue store k1 + getValue store k2, [k1, k2])

    Ackermann 0 n -> return (n + 1, [])
    Ackermann m 0 -> do
        let dep | m < 0     = AckermannNegative1
                | otherwise = Ackermann (m - 1) 0
        return (getValue store dep, [dep])
    Ackermann m n -> do
        let deps | m < 0     = [AckermannNegative1]
                 | n < 0     = [AckermannNegative2]
                 | otherwise = let k1 = Ackermann m (n - 1)
                                   k2 = Ackermann (m - 1) (getValue store k1)
                               in [k2, k1] -- We lookup the value of the head key
        return (getValue store (head deps), deps)

    -- All other keys correspond to inputs
    _ -> defaultCompute store key
