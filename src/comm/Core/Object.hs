{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}


module Core.Object where


import Core.Configuration
import Core.Coordinate


data HumanPose = ForwardsWedgedArms | ParalleledArms deriving (Eq, Ord)

deriving instance EnabledFlag 'NeedSerialization => Read HumanPose
deriving instance EnabledFlag 'NeedSerialization => Show HumanPose

data Object a b
         = Human (Position a) b HumanPose
         | StaticObject (StaticObject a b)

data StaticObject a b
         = EphemeralObject a (Position b)
         | PersistentObject a (Position b)
  deriving Eq

deriving instance (Read a, Read b, EnabledFlag 'NeedSerialization)
    =>
    Read (StaticObject a b)
deriving instance (Show a, Show b, EnabledFlag 'NeedSerialization)
    =>
    Show (StaticObject a b)

data StaticPose
