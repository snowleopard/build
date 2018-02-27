{-# LANGUAGE ConstraintKinds, DeriveFunctor, FlexibleContexts #-}
import Control.Monad
import Control.Monad.IO.Class
import Data.Map
import Data.Functor.Identity

import Development.Build
import Development.Build.Plan hiding (inputs)
import Development.Build.Store

import Development.Build.Example.Expression

inputs :: Map Key (Value Integer)
inputs = fromList [ (Variable "a", Value 3)
                  , (Variable "b", Value 4) ]

outputs :: Outputs Key
outputs = [ Ackermann (-10) 1
          , Increment (Variable "b")
          , Add (Variable "a") (Increment (Variable "b"))
          , Ackermann 3 3 ]

build :: (Monad m, Get m Key (Value Integer), Put m Key (Value Integer))
      => m (State Key (Value Integer), Plan Key (Value Integer))
build = dumbBuild compute outputs (State, noPlan)

tracingBuild :: (MonadIO m, Get m Key (Value Integer), Put m Key (Value Integer))
      => m (State Key (Value Integer), Plan Key (Value Integer))
tracingBuild = dumbTracingBuild compute outputs (State, noPlan)

result :: Map Key (Value Integer)
result = snd $ runIdentity $ runMapStore build KeyNotFound inputs

tracingResult :: IO (Map Key (Value Integer))
tracingResult = snd <$> runMapStore tracingBuild KeyNotFound inputs

evalutate :: Map Key (Value Integer) -> Key -> Value Integer
evalutate store key = findWithDefault (KeyNotFound key) key store

printOutputs :: Map Key (Value Integer) -> IO ()
printOutputs store = forM_ outputs $
    \key -> putStrLn (show key ++ " = " ++ show (evalutate store key))

main :: IO ()
main = do
    putStrLn "======== dumbBuild ========"
    printOutputs result
    putStrLn "======== dumbTracingBuild ========"
    printOutputs =<< tracingResult
