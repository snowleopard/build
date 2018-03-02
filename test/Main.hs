{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
import Control.Monad
import Control.Monad.IO.Class
import Data.Map
import Data.Functor.Identity

import Development.Build
import Development.Build.Plan hiding (inputs)
import Development.Build.Store

import Development.Build.Example.Expression

inputs :: Map Cell Int
inputs = fromList [ ("A1", 1)
                  , ("A2", 2)
                  , ("A3", 3) ]

-- TODO: Handle lookup errors nicer
cellNotFound :: Cell -> Int
cellNotFound cell = error $ "Cell not found: " ++ show cell

spreadsheet :: Spreadsheet
spreadsheet (Cell id) = case id of
    "B1" -> Just $ 1                     --          1
    "B2" -> Just $ "B1" + 1              -- 1 + 1 == 2
    "B3" -> Just $ "A3" * abs "B2"       -- 3 * 2 == 6
    "C1" -> Just $ IfZero "B3" "C2" 1000 --          1000
    "C2" -> Just $ IfZero "B3" 2000 "C1" --          1000
    "C3" -> Just $ Random 1 6            --          1..6
    _    -> Nothing

outputs :: Outputs Cell
outputs = [ "B1", "B2", "B3", "C1", "C2" ]

goDumb :: (Monad m, Get m Cell Int, Put m Cell Int) => m (State Cell Int, Plan Cell Int)
goDumb = dumbBuild (compute spreadsheet) outputs (State, noPlan)

goSlow :: (Monad m, Get m Cell Int, Put m Cell Int) => m (State Cell Int, Plan Cell Int)
goSlow = slowBuild (compute spreadsheet) outputs (State, noPlan)

goTracingDumb :: (MonadIO m, Get m Cell Int, Put m Cell Int) => m (State Cell Int, Plan Cell Int)
goTracingDumb = dumbTracingBuild (compute spreadsheet) outputs (State, noPlan)

result :: Map Cell Int
result = snd $ runIdentity $ runMapStore goDumb cellNotFound inputs

tracingResult :: IO (Map Cell Int)
tracingResult = snd <$> runMapStore goTracingDumb cellNotFound inputs

slowResult :: Map Cell Int
slowResult = snd $ runIdentity $ runMapStore goSlow cellNotFound inputs

evalutate :: Map Cell Int -> Cell -> Int
evalutate store key = findWithDefault (cellNotFound key) key store

printOutputs :: Map Cell Int -> IO ()
printOutputs store = forM_ outputs $
    \key -> putStrLn (show key ++ " = " ++ show (evalutate store key))

main :: IO ()
main = do
    putStrLn "======== dumbBuild ========"
    printOutputs result
    putStrLn "======== dumbTracingBuild ========"
    printOutputs =<< tracingResult
    putStrLn "======== slowBuild ========"
    printOutputs slowResult
