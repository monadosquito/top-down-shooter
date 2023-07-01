module Core.TriggerTest where


import Core.Trigger
import Core.Vector
import Core.VectorTest ()

import Test.QuickCheck


instance (Arbitrary a) => Arbitrary (DotProduct a) where
    arbitrary = DotProduct <$> arbitrary

instance Arbitrary InsideTriggerAction where
    arbitrary = oneof [pure KeepEnemiesAway, pure KeepEveryoneAway]

instance (Arbitrary a) => Arbitrary (Radius a) where
    arbitrary = Radius <$> arbitrary

instance
    (Arbitrary a, Num a, Arbitrary b, Num b)
    =>
    Arbitrary (Trigger a b) where
        arbitrary
            =
            oneof
                [ CellRectangleTrigger <$> arbitrary <*> arbitrary
                , CellSquareTrigger <$> arbitrary <*> arbitrary
                , CellTrigger <$> arbitrary
                , CircleTrigger <$> arbitrary <*> arbitrary
                , DotProductTrigger <$> arbitrary
                ]

dotProdAddHasId :: DotProduct Int -> Bool
dotProdAddHasId dotProd
    =
    dotProd <> mempty == dotProd && mempty <> dotProd == dotProd

dotProdAddIsAssoc :: DotProduct Int -> DotProduct Int -> DotProduct Int -> Bool
dotProdAddIsAssoc dotProdA dotProdB dotProdC
    =
    (dotProdA <> dotProdB) <> dotProdC == dotProdA <> (dotProdB <> dotProdC)

dotProdAddIsCommut :: DotProduct Int -> DotProduct Int -> Bool
dotProdAddIsCommut dotProdA dotProdB
    =
    dotProdA <> dotProdB == dotProdB <> dotProdA

rAddIsAssoc :: Radius Int -> Radius Int -> Radius Int -> Bool 
rAddIsAssoc rA rB rC = (rA <> rB) <> rC == rA <> (rB <> rC)

rAddIsCommut :: Radius Int -> Radius Int -> Bool
rAddIsCommut rA rB = rA <> rB == rB <> rA

rAddHasId :: Radius Int -> Bool 
rAddHasId r = r <> mempty == mempty <> r

trims :: Trigger Int Int -> Bool
trims cellRectTrig@(CellRectangleTrigger sCell eCell@(Vector2 eRow eClm))
    | sCell == eCell = trim cellRectTrig == CellTrigger sCell
    | eRow == eClm = trim cellRectTrig == CellSquareTrigger sCell eRow
    | otherwise = trim cellRectTrig == cellRectTrig
trims trig = trim trig == trig
