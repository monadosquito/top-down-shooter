{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}


module Act where


import Core.Trigger

import Data.Kind
import Numeric.Natural
import Text.Read
import qualified Control.Applicative as Applic
import qualified Miso.String as Ms


type Act :: Type -> Type -> Bool -> Type
data Act a b isAlsoDomAct where
    AddTrig :: (Show a, Show b) => TrigStepness a b -> Act a b 'True
    AddTrigGrp :: TriggerGroupCommon Double trigGrpCrit -> Act Double b 'True
    AltSlctedLiflPrefRunSpd :: SlctedLiflPrefRunSpd -> Act a b 'False
    ChngPrefName :: StrPrefName -> Act a b 'False
    DelPref :: MsPrefName -> Act a b 'False
    DelTrig :: TrigIx -> Act a b 'False
    Init :: Act a b 'False
    Loop :: Act a b 'False
    RlsTrigGrp :: Act a b 'False
    SavePref :: Act a b 'False
    SlctPref :: MsPrefName -> Act a b 'False
    SlctPrefType :: MsPrefName -> Act a b 'False
    UnsafeSlctHumanPose :: HumanPose -> Act a b 'False
    UnsafeSlctPrefBodyEdAct :: PrefBodyEdAct -> Act a b 'False
    UnsafeSlctSingTrig :: SingTrig -> Act a b 'False
    UnsafeSlctXAct :: InsTrigAct -> Act a b 'False
    View :: Act a b 'True

instance (Eq a, Eq b) => Eq (Act a b 'True) where
    AddTrig trigA == AddTrig trigB = trigA == trigB
    AddTrigGrp (SameVerticalSizeGroupCommon sameVertSizeGrpCommA)
        ==
        AddTrigGrp (SameVerticalSizeGroupCommon sameVertSizeGrpCommB)
        =
        sameVertSizeGrpCommA == sameVertSizeGrpCommB
    View == View = True
    _ == _ = False

data Act'
         = AlsoJsAct (Act Double Int 'True)
         | OnlySimpAct (Act Double Int 'False)
deriving instance Show a => Show (Act a b isAlsoDomAct)
instance (Read b, Show b) => Read (Act Double b 'True) where
    readPrec
        =
        parens
            $ lexP
            >>= \case
                    Ident "AddTrig" -> AddTrig <$> readPrec
                    Ident "AddTrigGrp"
                        ->
                        AddTrigGrp
                        <$> (readPrec
                             :: Read a
                             => ReadPrec
                                    (TriggerGroupCommon a 'SameVerticalSize)
                            )
                        Applic.<|> AddTrigGrp
                        <$> (readPrec
                             :: Read a
                             => ReadPrec
                                    (TriggerGroupCommon a 'SameHorizontalSize)
                            )
                    _ -> Applic.empty

data TrigStepness a b
         = OneSteppedSameHSizeUnitSectTrig (UnitDotProduct a)
         | OneSteppedSameVSizeUnitSectTrig (UnitDotProduct a)
         | OneSteppedSingTrig (SingleTrigger a b)
         | XSteppedUnitSectTrig
               (Maybe (UnitDotProduct a), Maybe (UnitDotProduct a))
  deriving (Eq, Read, Show)


type HumanPose = Ms.MisoString
type InsTrigAct = Ms.MisoString
type PrefBodyEdAct = Ms.MisoString
type MsPrefName = Ms.MisoString
type SingTrig = Ms.MisoString
type SlctedLiflPrefRunSpd = Ms.MisoString
type SlctedPrefW = Ms.MisoString
type StrPrefName = String
type TrigIx = Natural
