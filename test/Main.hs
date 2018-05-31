{-# LANGUAGE OverloadedStrings, RankNTypes, ConstraintKinds #-}
import Control.Monad
import Data.Bool
import Data.List.Extra
import Data.Maybe
import System.Exit

import qualified Data.Map as Map

import Build
import Build.Task
import Build.Rebuilder
import Build.Store
import Build.System

import Spreadsheet
import Examples()

type MultiBuild c i k v = Tasks c k v -> [k] -> Store i k v -> Store i k v

sequentialMultiBuild :: Build Monad i k v -> MultiBuild Monad i k v
sequentialMultiBuild build task outputs store = case outputs of
    []     -> store
    (k:ks) -> sequentialMultiBuild build task ks (build task k store)

sequentialMultiBuildA :: Build Applicative i k v -> MultiBuild Applicative i k v
sequentialMultiBuildA build task outputs store = case outputs of
    []     -> store
    (k:ks) -> sequentialMultiBuildA build task ks (build task k store)

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

tasks :: Tasks Monad Cell Int
tasks = spreadsheetTask spreadsheet

tasksA :: Tasks Applicative Cell Int
tasksA = spreadsheetTaskA acyclicSpreadsheet

test :: String -> Build Monad i Cell Int -> i -> IO Bool
test name build i = do
    let store   = inputs i
        result  = sequentialMultiBuild build tasks targets store
        correct = all (correctBuild tasks store result) targets
    putStr $ name ++ " is "
    case (trim name, correct) of
        ("dumb", False) -> do putStr "incorrect, which is [OK]\n"; return True
        (_     , False) -> do putStr "incorrect: [FAIL]\n"       ; return False
        (_     , True ) -> do putStr "correct: [OK]\n"           ; return True

testA :: String -> Build Applicative i Cell Int -> i -> IO Bool
testA name build i = do
    let store   = inputs i
        result  = sequentialMultiBuildA build tasksA targets store
        correct = all (correctBuild tasks store result) targets
    putStrLn $ name ++ " is " ++ bool "incorrect: [FAIL]" "correct: [OK]" correct
    return correct

testSuite :: IO Bool
testSuite = and <$> sequence
    [ test  "dumb      " dumb       ()
    , test  "busy      " busy       ()
    , test  "memo      " memo       ()
    , testA "make      " make       (Map.empty, 0)
    , testA "ninja     " ninja      mempty
    , test  "excel     " excel      ((dirtyKeys, unknowns), mempty)
    , test  "shake     " shake      mempty
    , test  "bazel     " bazel      mempty
    , test  "cloudShake" cloudShake mempty
    , testA "buck      " buck       mempty
    , test  "nix       " nix        mempty ]
  where
    dirtyKeys = Map.fromList [ (k, Dirty) | k <- targets ]
    unknowns k | isNothing (spreadsheet k) = Input
               | otherwise                 = Unknown


main :: IO ()
main = do
    success <- testSuite
    unless success $ die "\n========== At least one test failed! ==========\n"
