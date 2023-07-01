module Core.PrefabTest where


import Core.Prefab
import Core.ReactionTest ()
import Core.TriggerTest ()
import Core.VectorTest ()

import Test.QuickCheck


instance (Arbitrary b) => Arbitrary (PrefabIdentity b) where
    arbitrary = PrefabIdentity <$> arbitrary <*> arbitrary

instance Arbitrary EphemeralProperties where
    arbitrary = EphemeralProperties <$> arbitrary <*> arbitrary

instance Arbitrary LifefullPrefab where
    arbitrary = LifefullPrefab <$> arbitrary <*> arbitrary

instance Arbitrary StaticPrefab where
    arbitrary
        =
        oneof
            [ EphemeralPrefab <$> arbitrary <*> arbitrary
            , PersistentPrefab <$> arbitrary
            ]

instance Arbitrary Prefab where
    arbitrary
        =
        oneof
            [ DynamicPrefab <$> arbitrary
            , StaticPrefab <$> arbitrary
            , StaticPrefab <$> arbitrary
            ]


transs :: Prefab -> Prefab -> Bool
transs
    fromPref@(DynamicPrefab (LifefullPrefab _ identity))
    toPref@(StaticPrefab (PersistentPrefab _))
    =
    transform fromPref toPref == StaticPrefab (PersistentPrefab identity)
transs
    fromPref@(DynamicPrefab (LifefullPrefab ephemeralProperties identity))
    toPref@(StaticPrefab (EphemeralPrefab _ _))
    =
    transform fromPref toPref
    == StaticPrefab (EphemeralPrefab ephemeralProperties identity)
transs
    fromPref@(StaticPrefab (EphemeralPrefab ephemeralProperties identity))
    toPref@(DynamicPrefab (LifefullPrefab _ _))
    =
    transform fromPref toPref
    == DynamicPrefab (LifefullPrefab ephemeralProperties identity)

transs
    fromPref@(StaticPrefab (PersistentPrefab identity))
    toPref@(DynamicPrefab (LifefullPrefab _ _))
    =
    transform fromPref toPref
    == DynamicPrefab (LifefullPrefab (EphemeralProperties 0 0) identity)
transs
    fromPref@(StaticPrefab (PersistentPrefab identity))
    toPref@(StaticPrefab (EphemeralPrefab _ _))
    =
    transform fromPref toPref
    == StaticPrefab (EphemeralPrefab (EphemeralProperties 0 0) identity)
transs fromPref toPref = transform fromPref toPref == fromPref
