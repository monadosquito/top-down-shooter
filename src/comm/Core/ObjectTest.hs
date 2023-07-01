module Core.ObjectTest where


import Core.Object

import Test.QuickCheck


instance Arbitrary HumanPose where
    arbitrary = oneof [pure ForwardsWedgedArms, pure ParalleledArms]
