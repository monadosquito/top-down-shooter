{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}


module Core.Reaction where


import Core.Configuration
import Core.Object
import Core.Trigger


data Reaction = Reaction PlayerIsInsideTriggerAction HumanPose

deriving instance EnabledFlag 'NeedComparison => Eq Reaction
deriving instance EnabledFlag 'NeedComparison => Ord Reaction
deriving instance EnabledFlag 'NeedSerialization => Read Reaction
deriving instance EnabledFlag 'NeedSerialization => Show Reaction
