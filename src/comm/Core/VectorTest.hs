module Core.VectorTest where


import Core.Vector

import Test.QuickCheck


instance (Arbitrary a, Num a) => Arbitrary (Vector2 a) where
    arbitrary = Vector2 <$> arbitrary <*> arbitrary

addHasId :: Vector2 Int -> Bool
addHasId vec = vec <> mempty == vec && mempty <> vec == vec

addIsAssoc :: Vector2 Int -> Vector2 Int -> Vector2 Int -> Bool
addIsAssoc vecA vecB vecC = (vecA <> vecB) <> vecC == vecA <> (vecB <> vecC)

addIsCommut :: Vector2 Int -> Vector2 Int -> Bool
addIsCommut vecA vecB = vecA <> vecB == vecB <> vecA

functorComposes :: Fun Int Int -> Fun Int Int -> Vector2 Int -> Bool
functorComposes (Fun _ g) (Fun _ f) vec2
    =
    fmap (g . f) vec2 == (fmap g . fmap f) vec2

functorHasId :: Vector2 Int -> Bool
functorHasId vec2 = fmap id vec2 == id vec2

pythagoreanThrm :: Vector2 Double -> Bool
pythagoreanThrm vec2@(Vector2 legA legB)
    =
    squaredLength vec2 == legA ^ (2 :: Int) + legB ^ (2 :: Int)
