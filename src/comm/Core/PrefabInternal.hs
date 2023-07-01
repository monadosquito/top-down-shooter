{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}


module Core.PrefabInternal where


import Core.Configuration


newtype RunSpeed a = RunSpeed a deriving Eq

instance Num s => Semigroup (RunSpeed s) where
    RunSpeed runSpeedA <> RunSpeed runSpeedB = RunSpeed (runSpeedA + runSpeedB)

instance Num s => Monoid (RunSpeed s) where
    mempty = RunSpeed 0

deriving instance (Read a, EnabledFlag 'NeedSerialization)
    =>
    Read (RunSpeed a)
deriving instance (Show a, EnabledFlag 'NeedSerialization)
    =>
    Show (RunSpeed a)
