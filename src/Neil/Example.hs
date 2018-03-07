{-# LANGUAGE Rank2Types, ConstraintKinds, DeriveFunctor, GADTs, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Neil.Example where

import Data.Default
import Neil.Builder
import Neil.Compute
import Neil.Build
import Control.Monad
import qualified Data.Map as Map

data Add k v = Add k k
             | Source

runAdd :: Num v => (k -> Add k v) -> Compute Applicative k v
runAdd f ask k = case f k of
    Source -> Nothing
    Add a b -> Just $ ((+) <$> ask a <*> ask b)


example "a" = Source
example "b" = Source
example "c" = Add "a" "b"
example "d" = Add "c" "c"

store0 = Map.fromList [("a",1),("b",2)]
store' = Map.insert "a" 3

test :: Show i => Build Applicative i String Int -> IO ()
test build = do
    let (info1, store1) = build (runAdd example) "d" Nothing store0
    let (info2, store2) = build (runAdd example) "d" (Just info1) (store' store1)
    print store1
    --print info1
    print store2
    --print info2

main = do
    test dumb
    test dumbDynamic
    test dumbTopological
    test dumbRecursive
    test make
    test makeTrace
    test makeDirtyBit
    test shake
    test shakeDirtyBit
    test spreadsheet
    test spreadsheetTrace
    test spreadsheetRemote
    test bazel
    test shazel
