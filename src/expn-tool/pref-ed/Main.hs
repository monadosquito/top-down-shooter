{-# LANGUAGE PolyKinds #-}


import Act
import Core.Prefab
import Mdl
import Upd
import View.Root
import WwwEnv

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Language.Javascript.JSaddle
import Miso


app :: JSM ()
app = do
    mCtxIoRef <- liftIO . newIORef
                     $ MCtx
                           { _adjustedTrigLike = Nothing
                           , _anim = return ()
                           , _animAct = OnlySimpAct Loop
                           , _mdl = mdl
                           , _scn = jsNull
                           }
    startApp
        App
            { events = defaultEvents
            , initialAction = OnlySimpAct Init
            , logLevel = Miso.Off
            , model = mdl :: Mdl
            , mountPoint = Nothing
            , subs = []
            , update = fromTransition . upd mCtxIoRef
            , view = runReader root
            }
  where
    _ = inclPub
    _ = loadPub

    mdl = Mdl
              { _slctedPlayerIsInsTrigAct = Nothing
              , _slctedPose = Nothing
              , _slctedPref = sPref
              , _slctedPrefBodyEdAct = View
              , _slctedPrefCopy = sPref
              , _slctedSingTrig = Nothing
              , _trigGrpingIsOn = False
              }
    sPref = StaticPrefab (PrefabIdentity mempty mempty)
