{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
import Control.Monad
import Control.Applicative
import Data.Bool
import Data.List.Extra
import Data.Maybe
import System.Exit

import Build
import Build.Task
import Build.Task.Monad (correctBuild)
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

targets :: [Cell]
targets = [ "A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "F0", "F1", "F4" ]

task :: Task Monad Cell Int
task = spreadsheetTask spreadsheet

taskA :: Task Applicative Cell Int
taskA = spreadsheetTaskA acyclicSpreadsheet

test :: String -> Build Monad i Cell Int -> i -> IO Bool
test name build i = do
    let store   = inputs i
        result  = sequentialMultiBuild build task targets store
        correct = all (correctBuild task store result) targets
    putStr $ name ++ " is "
    case (trim name, correct) of
        ("dumb", False) -> do putStr "incorrect, which is [OK]\n"; return True
        (_     , False) -> do putStr "incorrect: [FAIL]\n"       ; return False
        (_     , True ) -> do putStr "correct: [OK]\n"           ; return True

testA :: String -> Build Applicative i Cell Int -> i -> IO Bool
testA name build i = do
    let store   = inputs i
        result  = sequentialMultiBuildA build taskA targets store
        correct = all (correctBuild task store result) targets
    putStrLn $ name ++ " is " ++ bool "incorrect: [FAIL]" "correct: [OK]" correct
    return correct

instance Timed Time where
    mtime = id

{- Inspired from the following Makefile:

	# Disable builtin rules for a cleaner `make -d`
	MAKEFLAGS += --no-builtin-rules

	# touch -t [[CC]YY]MMDDhhmm[.ss] <file>
	MMDDhh = 010101

	.PHONY: all
	all: y z

	x:
		touch x -t $(MMDDhh)03

	y: x
		touch y -t $(MMDDhh)02

	z: x
		touch z -t $(MMDDhh)01

First run: everything gets built (as usual), but x is built only once (there is a topological sort in the execution)
$ make all
touch x -t 01010103
touch y -t 01010102
touch z -t 01010101

Second run: y and z are built again, because they are out of date (they always are by construction).
$ make all
touch y -t 01010102
touch z -t 01010101
-}
taskMakeTrick :: Task Applicative String Time
taskMakeTrick get k = case k of
    "x" -> Just $ (const 3) <$> phony []
    "y" -> Just $ (const 2) <$> phony ["x"]
    "z" -> Just $ (const 1) <$> phony ["x"]
    "all" -> Just $ phony ["y", "z"]
    _   -> Nothing
  where
    phony = foldl (liftA2 seq) (pure (-1)) . map get

testATimed :: String -> Build Applicative () String Time -> IO Bool
testATimed name build = do
    let store   = initialise () (\_ -> -1)
        result  = sequentialMultiBuildA build taskMakeTrick ["all", "all"] store
        correct = all (correctBuild taskMakeTrick store result) ["y", "z"]
    putStrLn $ name ++ " is " ++ bool "incorrect: [FAIL]" "correct: [OK]" correct
    return correct

testSuite :: IO Bool
testSuite = and <$> sequence
    [ test  "dumb      " dumb       ()
    , test  "busy      " busy       ()
    , test  "memo      " memo       ()
    , testA "make      " make       (const (-1), 0)
    , testATimed "makeT     " makeT
    , testA "ninja     " ninja      mempty
    , test  "excel     " excel      ((const True, mempty), mempty)
    , test  "shake     " shake      mempty
    , test  "cloudShake" cloudShake mempty
    , testA "bazel     " bazel      mempty
    , testA "buck      " buck       mempty ]
    -- Fails currently:
    -- , test  "nix       " nix        mempty ]

main :: IO ()
main = do
    success <- testSuite
    unless success $ die "\n========== At least one test failed! ==========\n"
