{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
import Control.Monad
import Data.Map

import Development.Build
import Development.Build.Store

import Development.Build.Example.Spreadsheet

inputs :: Map Cell Int
inputs = fromList [ ("A1", 1)
                  , ("A2", 2)
                  , ("A3", 3) ]

spreadsheet :: Spreadsheet
spreadsheet cell = case name cell of
    "B1"  -> Just $ 1                       --          1
    "B2"  -> Just $ "B1" + 1                -- 1 + 1 == 2
    "B3"  -> Just $ "A3" * abs "B2"         -- 3 * 2 == 6
    "C1"  -> Just $ IfZero "B3" "C2" 1000   --          1000
    "C2"  -> Just $ IfZero "B3" 2000 "C1"   --          1000
    "C3"  -> Just $ Random 1 6              --          1..6
    "F0"  -> Just $ 0                       --          0
    "F1"  -> Just $ 1                       --          1
    'F':_ -> Just $ rel (-1) 0 + rel (-2) 0 --          Fn = F(n - 1) + F(n - 2)
    _     -> Nothing

outputs :: [Cell]
outputs = [ "B1", "B2", "B3", "C1", "C2", "F30" ]

-- TODO: Handle lookup errors nicer
cellNotFoundError :: Cell -> Int
cellNotFoundError cell = error $ "Cell not found: " ++ show cell

-- TODO: Handle lookup errors nicer
cellNotFoundValue :: Cell -> Int
cellNotFoundValue _ = 0

dumbResult :: Map Cell Int
dumbResult = snd $ dumb cellNotFoundValue (compute spreadsheet) outputs Nothing inputs

slowResult :: Map Cell Int
slowResult = snd $ slow cellNotFoundValue (compute spreadsheet) outputs Nothing inputs

tracingDumbResult :: IO (Map Cell Int)
tracingDumbResult = snd <$> runMapStoreT build cellNotFoundValue inputs
  where
    build = dumbTracing (compute spreadsheet) outputs

evalutate :: Map Cell Int -> Cell -> Int
evalutate store key = findWithDefault (cellNotFoundError key) key store

printOutputs :: Map Cell Int -> IO ()
printOutputs store = forM_ outputs $
    \key -> putStrLn (show (name key) ++ " = " ++ show (evalutate store key))

main :: IO ()
main = do
    putStrLn "======== dumbBuild ========"
    printOutputs dumbResult
    putStrLn "======== dumbTracingBuild ========"
    printOutputs =<< tracingDumbResult
    putStrLn "======== slowBuild ========"
    printOutputs slowResult
