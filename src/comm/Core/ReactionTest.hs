module Core.ReactionTest where


import Core.ObjectTest ()
import Core.Reaction
import Core.TriggerTest ()

import Test.QuickCheck


instance Arbitrary Reaction where
    arbitrary = Reaction <$> arbitrary <*> arbitrary
