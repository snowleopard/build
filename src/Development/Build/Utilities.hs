module Development.Build.Utilities (
    -- * Graph operations
    reachable, topSort, reach, reachM,

    -- * Transformers
    AltConst (..),

    -- * Logic combinators
    forall, forallM, exists, existsM, (==>)
    ) where

import Control.Applicative
import Data.Functor.Identity

-- TODO: Implement
reachable :: Eq a => (a -> [a]) -> a -> [a]
reachable = undefined

topSort :: Eq a => (a -> [a]) -> [a] -> [a]
topSort = undefined

-- TODO: Just switch to a proper graph library for reachability analysis.
-- | Given a function to compute successors of a vertex, apply it recursively
-- starting from a given vertex. Returns @Nothing@ if this process does not
-- terminate because of cycles. Note that the current implementation is very
-- inefficient: it trades efficiency for simplicity. The resulting list is
-- likely to contain an exponential number of duplicates.
reach :: Eq a => (a -> [a]) -> a -> Maybe [a]
reach successors = runIdentity . reachM (return . successors)

-- | Given a monadic function to compute successors of a vertex, apply it
-- recursively starting from a given vertex. Returns @Nothing@ if this process
-- does not terminate because of cycles. Note that the current implementation is
-- very inefficient: it trades efficiency for simplicity. The resulting list is
-- likely to contain an exponential number of duplicates.
reachM :: (Eq a, Monad m) => (a -> m [a]) -> a -> m (Maybe [a])
reachM successors a = fmap (filter (/= a)) <$> go [] a
  where
    go xs x | x `elem` xs = return Nothing -- A cycle is detected
            | otherwise   = do res <- traverse (go (x:xs)) =<< successors x
                               return $ ((x:xs)++) . concat <$> sequence res

-- TODO: We can probably achieve the same effect using Compose [] Const.
newtype AltConst a b = AltConst { getAltConst :: [[a]] }

instance Functor (AltConst a) where
    fmap _ (AltConst xs) = AltConst xs

instance Applicative (AltConst a) where
    pure _ = AltConst [[]]
    AltConst xs <*> AltConst ys = AltConst [ x ++ y | x <- xs, y <- ys ]

instance Alternative (AltConst a) where
    empty = AltConst []
    AltConst xs <|> AltConst ys = AltConst (xs ++ ys)

-- | Check that a predicate holds for all values of @a@.
forall :: (a -> Bool) -> Bool
forall = undefined

-- | Check that a monadic predicate holds for all values of @a@.
forallM :: (a -> m Bool) -> m Bool
forallM = undefined

-- | Check that a predicate holds for some value of @a@.
exists :: (a -> Bool) -> Bool
exists = undefined

-- | Check that a monadic predicate holds for some value of @a@.
existsM :: (a -> m Bool) -> m Bool
existsM = undefined

-- | Logical implication.
(==>) :: Bool -> Bool -> Bool
x ==> y = not x || y

infixr 0 ==>
