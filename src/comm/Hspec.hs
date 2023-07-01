import Core.PrefabTest
import Core.VectorTest
import Core.TriggerTest

import Test.Hspec
import Test.Hspec.QuickCheck


main :: IO ()
main
    =
    hspec $ do
        describe "Dot product addition" $ do
            prop "is associative" dotProdAddIsAssoc
            prop "is commutative" dotProdAddIsCommut
            prop "has an identity element" dotProdAddHasId
        describe "Prefab transform"
            $ prop
                  (unwords
                       [ "transforms"
                       , "the first prefab"
                       , "into the second one"
                       , "preserving identity"
                       ]
                  )
                  transs
        describe "Radius addition" $ do
            prop "is associative" rAddIsAssoc
            prop "is commutative" rAddIsCommut
            prop "has an identity element" rAddHasId
        describe "Trigger trim"
            $ prop
                  "trims a trigger from redundant data"
                  trims
        describe "Vector as a functor" $ do
            prop "preserves composition" functorComposes
            prop "preserves identity" functorHasId
        describe "Vector2 as a triangle"
            $ prop "follows Pythagorean theorem" pythagoreanThrm
        describe "Vector2 addition" $ do
            prop "is associative" addIsAssoc
            prop "is commutative" addIsCommut
            prop "has an identity element" addHasId
