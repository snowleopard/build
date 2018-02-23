import Control.Monad
import Data.Map

import Development.Build
import Development.Build.Plan hiding (inputs)
import Development.Build.Store

import Development.Build.Example.Expression

inputs :: Map Key (Value Integer)
inputs = fromList [ (Variable "a", Value 3)
                  , (Variable "b", Value 5) ]

outputs :: Outputs Key
outputs = [ Ackermann (-10) 1
          , Add (Variable "a") (Variable "b")
          , Ackermann 3 3 ]

result :: Map Key (Value Integer)
result = snd $ runMapStore (dumbBuild compute outputs (State, noPlan)) KeyNotFound inputs

evalutate :: Key -> Value Integer
evalutate key = findWithDefault (KeyNotFound key) key result

main :: IO ()
main = forM_ outputs $ \key -> putStrLn(show key ++ " = " ++ show (evalutate key))
