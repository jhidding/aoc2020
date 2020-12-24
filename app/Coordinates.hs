module Coordinates where

import RIO

data Coordinates a = Coordinates a a
    deriving (Show, Eq)

instance Functor Coordinates where
    fmap f (Coordinates x y) = Coordinates (f x) (f y)

instance Applicative Coordinates where
    pure n = Coordinates n n
    liftA2 f (Coordinates x1 y1) (Coordinates x2 y2)
        = Coordinates (f x1 x2) (f y1 y2)

instance Num a => Num (Coordinates a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    signum = fmap signum
    abs = fmap abs
    fromInteger = pure . fromInteger

instance Num a => Semigroup (Coordinates a) where
    (Coordinates x1 y1) <> (Coordinates x2 y2) = Coordinates (x1 + x2) (y1 + y2)

instance Num a => Monoid (Coordinates a) where
    mempty = Coordinates 0 0

-- instance Eq a => Eq (Coordinates a) where
--     (Coordinates x1 y1) == (Coordinates x2 y2) = x1 == x2 && y1 == y2

instance Ord a => Ord (Coordinates a) where
    compare (Coordinates x1 y1) (Coordinates x2 y2)
        = case compare y1 y2 of
            EQ -> compare x1 x2
            cy -> cy

toTuple :: Coordinates a -> (a, a)
toTuple (Coordinates x y) = (x, y)

fromTuple :: (a, a) -> Coordinates a
fromTuple (x, y) = Coordinates x y
