{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
import Control.Monad
import Data.Maybe

import Build
import Build.Task
import Build.Store
import Build.System

import Build.Example.Spreadsheet

inputs :: i -> Store i Cell Int
inputs i = initialise i $ \cell -> fromMaybe 0 $ lookup cell
    [ ("A1", 1)
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

acyclicSpreadsheet :: Spreadsheet
acyclicSpreadsheet cell = case name cell of
    "B1"  -> Just $ 1                       --          1
    "B2"  -> Just $ "B1" + 1                -- 1 + 1 == 2
    "B3"  -> Just $ "A3" * abs "B2"         -- 3 * 2 == 6
    "C1"  -> Just $ IfZero "B3" "B2" 1000   --          1000
    "C2"  -> Just $ IfZero "B3" 2000 "C1"   --          1000
    "C3"  -> Just $ Random 1 6              --          1..6
    "F0"  -> Just $ 0                       --          0
    "F1"  -> Just $ 1                       --          1
    'F':_ -> Just $ rel (-1) 0 + rel (-2) 0 --          Fn = F(n - 1) + F(n - 2)
    _     -> Nothing

outputs :: [Cell]
outputs = [ "B1", "B2", "B3", "C1", "C2", "F30" ]

task :: Task Monad Cell Int
task = spreadsheetTask spreadsheet

taskA :: Task Applicative Cell Int
taskA = spreadsheetTaskA acyclicSpreadsheet

printOutputs :: Store i Cell Int -> IO ()
printOutputs store = do
    forM_ outputs $
        \key -> putStrLn (show (name key) ++ " = " ++ show (getValue key store))

    -- putStrLn $ "Final info = " ++ show (getInfo store)

test :: i -> Build Monad i Cell Int -> Store i Cell Int
test i build = sequentialMultiBuild build task outputs (inputs i)

testA :: i -> Build Applicative i Cell Int -> Store i Cell Int
testA i build = sequentialMultiBuildA build taskA outputs (inputs i)

main :: IO ()
main = do
    putStrLn "======== dumb ========"
    printOutputs (test () dumb)
    putStrLn "======== busy ========"
    printOutputs (test () busy)
    putStrLn "======== memo ========"
    printOutputs (test () memo)
    putStrLn "======== make ========"
    printOutputs (testA (const 0, 0) make)
    putStrLn "======== excel ========"
    printOutputs (test ((const True, mempty), mempty) excel)
    putStrLn "======== shake ========"
    printOutputs (test mempty shake)
    putStrLn "======== cloudShake ========"
    printOutputs (test mempty cloudShake)
    putStrLn "======== bazel ========"
    printOutputs (testA mempty bazel)
