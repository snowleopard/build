{-# LANGUAGE Rank2Types, ConstraintKinds, DeriveFunctor, GADTs, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Neil.Example where

import Data.Default
import Neil.Builder
import Neil.Compute
import Neil.Build
import Neil.Execute
import Control.Monad
import qualified Data.Map as Map

data Add k v = Add k k
             | Source

runAdd :: Num v => (k -> Add k v) -> Compute Applicative k v
runAdd f ask k = case f k of
    Source -> pure Nothing
    Add a b -> Just <$> ((+) <$> ask a <*> ask b)


example "a" = Source
example "b" = Source
example "c" = Add "a" "b"
example "d" = Add "c" "c"

store0 = Map.fromList [("a",1),("b",2)]
store' = Map.insert "a" 3

test :: Build Applicative String Int i -> IO ()
test build = do
    let (info1, store1) = build (runAdd example) ["d"] Nothing store0
    let (info2, store2) = build (runAdd example) ["d"] (Just info1) (store' store1)
    print store1
    print store2

main = do
    test dumb
    test dumbOnce
    test make
    test shake
    test spreadsheet
