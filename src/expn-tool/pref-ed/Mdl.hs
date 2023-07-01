{-# LANGUAGE DataKinds #-}


module Mdl where


import Core.Prefab
import Core.Trigger
import qualified Act
import qualified Core.Object as Obj

import Language.Javascript.JSaddle


data MCtx
         =
         MCtx
             { _adjustedTrigLike :: Maybe (AnyTriggerLike Double Int)
             , _anim :: JSM ()
             , _animAct :: Act.Act'
             , _mdl :: Mdl
             , _scn :: JSVal
             }

data Mdl = Mdl
               { _slctedPlayerIsInsTrigAct :: Maybe PlayerIsInsideTriggerAction
               , _slctedPose :: Maybe Obj.HumanPose
               , _slctedPref :: Prefab String Double Int
               , _slctedPrefBodyEdAct :: Act.Act Double Int 'True
               , _slctedPrefCopy :: Prefab String Double Int
               , _slctedSingTrig :: Maybe (SingleTrigger Double Int)
               , _trigGrpingIsOn :: Bool
               }
  deriving Eq
