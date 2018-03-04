{-# LANGUAGE Rank2Types, ConstraintKinds, DeriveFunctor, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Neil.Example where

import Data.Default
import Neil.Build
import Neil.Compute
import Neil.Constraints
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

store0 :: Map.Map String Int
store0 = Map.fromList [("a",1),("b",2)]

disp (Disk _ a b) = "Disk " ++ show a ++ " " ++ show b

test :: forall k v . (k ~ String, v ~ Int) => ((k -> [k]) -> [k] -> M k v ()) -> IO ()
test build = do
    let deps = getDependencies $ runAdd example
        d0 = Disk mempty (Map.map (def,) store0) mempty
        d1 = execute (build deps ["d"]) (runAdd example) d0
        d2 = execute (void $ putStore "a" 3) (runAdd example) d1
        d3 = execute (build deps ["d"]) (runAdd example) d2
    print $ disp d1
    print $ disp d3

main = do
    test $ const dumb
    test $ const dumbOnce
    test make
    test $ const shake
