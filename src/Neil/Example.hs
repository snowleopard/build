{-# LANGUAGE Rank2Types, ConstraintKinds, DeriveFunctor, GADTs, ScopedTypeVariables #-}

module Neil.Example where

import Neil.Build
import Neil.Compute
import Neil.Execute
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

store0 :: Map.Map String Int
store0 = Map.fromList [("a",1),("b",2)]
store' = Map.insert "a" 3

disp (Disk a b) = "Disk " ++ show a ++ " " ++ show b

test :: forall c k v . (k ~ String, v ~ Int) => ([k] -> M Applicative k v ()) -> IO ()
test build = do
    let d1 = execute (build ["d"]) (runAdd example) (Disk store0 mempty)
        d2 = execute (build ["d"]) (runAdd example) d1{diskStore = store' $ diskStore d1}
    print $ disp d1
    print $ disp d2    

main = do
    test dumb
    test dumbOnce
    -- test make
    test shake
