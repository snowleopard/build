import Development.Build
import Development.Build.Plan hiding (inputs)
import Development.Build.Store

import Development.Build.Example.Expression

inputs :: [(Key, Value Integer)]
inputs = [ (Variable "a", Value 3)
         , (Variable "b", Value 5) ]

initialStore :: ExpressionStore
initialStore = setValues inputs expressionStore

key1 :: Key
key1 = Ackermann (-10) 1

key2 :: Key
key2 = Add (Variable "a") (Variable "b")

key3 :: Key
key3 = Ackermann 3 3

finalStore :: ExpressionStore
finalStore = res
  where
    (_, _, res) = dumbBuild compute [key1, key2, key3] (State, noPlan, initialStore)

main :: IO ()
main = do
    putStrLn $ show key1 ++ " = " ++ show (getValue finalStore key1)
    putStrLn $ show key2 ++ " = " ++ show (getValue finalStore key2)
    putStrLn $ show key3 ++ " = " ++ show (getValue finalStore key3)
