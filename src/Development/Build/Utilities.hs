module Development.Build.Utilities (
    -- * Logic combinators
    forall, forallM, exists, existsM, (==>)
    ) where

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
x ==> y = if x then y else True

infixr 0 ==>
