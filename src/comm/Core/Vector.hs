{-# LANGUAGE StandaloneDeriving #-}


module Core.Vector where


import Prelude hiding (length, subtract)


data Vector2 a = Vector2 a a -- deriving (Eq, Read, Show)

deriving instance Eq a => Eq (Vector2 a)
deriving instance Show a => Show (Vector2 a)
deriving instance Read a => Read (Vector2 a)


instance (Num n) => Semigroup (Vector2 n) where
    Vector2 aX aY <> Vector2 bX bY = Vector2 (aX + bX) (aY + bY)

instance (Num n) => Monoid (Vector2 n) where
    mempty = Vector2 0 0

instance Functor Vector2 where
    fmap f (Vector2 componentA componentB)
        =
        Vector2 (f componentA) (f componentB)


distance :: (Floating f) => Vector2 f -> Vector2 f -> f
distance vector2 = sqrt . squaredLength . subtract vector2

squaredLength :: (Floating f) => Vector2 f -> f
squaredLength (Vector2 x y) = x ^ (2 :: Int) + y ^ (2 :: Int)

subtract :: (Num n) => Vector2 n -> Vector2 n -> Vector2 n
subtract vec = (<> fmap negate vec)
