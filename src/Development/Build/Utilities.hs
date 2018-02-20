module Development.Build.Utilities (forall, exists, (==>)) where

-- | Check that a predicate holds for all values of @a@.
forall :: (a -> Bool) -> Bool
forall = undefined

-- | Check that a predicate holds for some value of @a@.
exists :: (a -> Bool) -> Bool
exists = undefined

-- | Logical implication.
(==>) :: Bool -> Bool -> Bool
x ==> y = if x then y else True

infixr 0 ==>
