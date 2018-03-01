{-# LANGUAGE Rank2Types, ConstraintKinds, DeriveFunctor, GADTs #-}

module Neil.Example where

import Neil.Compute
import Neil.Types
import Neil.Implementation

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

files0 "a" = Just 1
files0 "b" = Just 2
files0 _ = Nothing

files' d "a" = Just 3
files' d x = d x

disp (Disk a b) = "Disk " ++ show a ++ " " ++ show [(a, b [a]) | a <- ['a'..'d']]

test :: (Show i, k ~ String, v ~ Int) => (Compute Monad k v -> [k] -> Disk i k v -> Disk i k v) -> i -> IO ()
test build start = do
    let d1 = build (runAdd example) ["d"] $ Disk start files0
        d2 = build (runAdd example) ["d"] $ d1{diskFiles = files' $ diskFiles d1}
    print $ disp d1
    print $ disp d2    

main = do
    test dumb ()
    test dumbOnce ()
--    test make ()
    test shake mempty
