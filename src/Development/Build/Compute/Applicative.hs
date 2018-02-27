{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes #-}
module Development.Build.Compute.Applicative (
    Script (..), normalForm, getScript, runScript
    ) where

import Development.Build.Store

data Script k v a where
    GetValue :: k -> Script k v v
    FMap     :: (a -> b) -> Script k v a -> Script k v b
    Pure     :: a -> Script k v a
    Ap       :: Script k v (a -> b) -> Script k v a -> Script k v b

-- TODO: Switch to vectors?
normalForm :: Script k v a -> ([k], [v] -> a)
normalForm (GetValue key ) = ([key], head)
normalForm (FMap f script) = (keys, f . g) where (keys, g) = normalForm script
normalForm (Pure a)        = ([], const a)
normalForm (Ap sf sa)      = (kf ++ ka, f)
  where
    (kf, ff) = normalForm sf
    (ka, fa) = normalForm sa
    f vs = (ff $ take (length kf) vs) (fa $ drop (length kf) vs)

instance Get (Script k v) k v where
    getValue = GetValue

instance Functor (Script k v) where
    fmap = FMap

instance Applicative (Script k v) where
    pure  = Pure
    (<*>) = Ap

getScript :: (forall f. (Applicative f, Get f k v) => k -> f v) -> k -> Script k v v
getScript = id

runScript :: (Applicative f, Get f k v) => (k -> Script k v v) -> k -> f v
runScript script key = f <$> traverse getValue ks
  where
    (ks, f) = normalForm (script key)
