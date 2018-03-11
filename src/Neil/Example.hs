{-# LANGUAGE Rank2Types, ConstraintKinds, DeriveFunctor, GADTs, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Neil.Example where

import Neil.Builder
import Neil.Compute
import Neil.Build
import Control.Monad
import Data.Maybe


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

store0 k = fromMaybe 0 $ lookup k [("a",1),("b",2)]
store' store k = if k == "a" then 3 else store k

showStore store = unwords [k ++ "=" ++ show (store k) | k <- ["a","b","c","d"]]

test :: Show i => Build Applicative i String Int -> IO ()
test build = do
    let (info1, store1) = build (runAdd example) "d" Nothing store0
    let (info2, store2) = build (runAdd example) "d" (Just info1) (store' store1)
    let res = showStore store0 ++ " ==> " ++ showStore store1 ++ " ==> " ++ showStore store2
    putStrLn res
    when (res /= "a=1 b=2 c=0 d=0 ==> a=1 b=2 c=3 d=6 ==> a=3 b=2 c=5 d=10") $ fail "did not match!"

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
