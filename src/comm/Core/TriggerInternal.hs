{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}


module Core.TriggerInternal where


import Core.Configuration


newtype UnitDotProduct a = UnitDotProduct a
instance Num n => Semigroup (UnitDotProduct n) where
    UnitDotProduct dotProductA <> UnitDotProduct dotProductB
        =
        UnitDotProduct (dotProductA + dotProductB)

instance Num n => Monoid (UnitDotProduct n) where
    mempty = UnitDotProduct 0

deriving instance (Eq a, EnabledFlag 'NeedComparison) => Eq (UnitDotProduct a)
deriving instance (Read a, EnabledFlag 'NeedSerialization)
    =>
    Read (UnitDotProduct a)
deriving instance (Show a, EnabledFlag 'NeedSerialization)
    =>
    Show (UnitDotProduct a)
