{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Development.Build.Example.Expression where

import Development.Build.Compute
import Development.Build.Store

data Key = Variable String
         | Add Key Key
         | Ackermann Integer Integer
         | AckermannNegativeM
         | AckermannNegativeN
         deriving (Eq, Ord, Show)

type Value = Integer

type ExpressionStore = Store Key Value

expressionStore :: ExpressionStore
expressionStore = mapStore

compute :: Compute Key Value
compute store key = case key of
    Add k1 k2 -> return (getValue store k1 + getValue store k2, [k1, k2])

    Ackermann 0 n -> return (n + 1, [])
    Ackermann m 0 -> do
        let dep | m < 0     = AckermannNegativeM
                | otherwise = Ackermann (m - 1) 0
        return (getValue store dep, [dep])
    Ackermann m n -> do
        let deps | m < 0     = [AckermannNegativeM]
                 | n < 0     = [AckermannNegativeN]
                 | otherwise = let k1 = Ackermann m (n - 1)
                                   k2 = Ackermann (m - 1) (getValue store k1)
                               in [k2, k1] -- We lookup the value of the head key
        return (getValue store (head deps), deps)

    -- All other keys correspond to inputs
    _ -> defaultCompute store key
