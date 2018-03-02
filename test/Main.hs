{-# LANGUAGE ConstraintKinds, DeriveFunctor, FlexibleContexts #-}
import Control.Monad
import Control.Monad.IO.Class
import Data.Map
import Data.Functor.Identity

import Development.Build
import Development.Build.Plan hiding (inputs)
import Development.Build.Store

import Development.Build.Example.Expression

inputs :: Map Cell Int
inputs = fromList [ (Cell 0 0, 0)
                  , (Cell 0 1, 1)
                  , (Cell 0 2, 2) ]

cellNotFound :: Cell -> Int
cellNotFound cell = error $ "Cell not found: " ++ show cell

spreadsheet :: Spreadsheet
spreadsheet (Cell x y) = case (x, y) of
    (1, 1) -> Just $ 1                                       --          1
    (1, 2) -> Just $ cell 1 1 + 1                            -- 1 + 1 == 2
    (1, 3) -> Just $ cell 0 2 * abs (cell 1 2)               -- 2 * 2 == 4
    (2, 0) -> Just $ IfZero (cell 1 3) (cell 2 1) (cell 0 0) --          0
    (2, 1) -> Just $ IfZero (cell 1 3) (cell 0 1) (cell 2 0) --          0
    (2, 2) -> Just $ Random 1 6                              --          1..6
    _      -> Nothing

outputs :: Outputs Cell
outputs = [ Cell 1 1
          , Cell 1 2
          , Cell 1 3
          , Cell 2 0
          , Cell 2 1 ]

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
