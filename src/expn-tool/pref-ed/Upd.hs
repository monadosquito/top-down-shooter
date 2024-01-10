{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}


module Upd where


import Act
import Cfg
import Core.Reaction
import Core.Trigger
import Lens
import Mdl
import Pref
import qualified Core.Prefab as Pref
import qualified Core.Vector as V
import qualified JsApi.Comm.Filt as JsApi
import qualified JsApi.Three.Filt as Three

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.List.Split
import Data.Maybe
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.EventTargetClosures
import GHCJS.DOM.ParentNode
import GHCJS.DOM.Types
import GHCJS.DOM.Window
import System.Directory
import qualified Control.Monad.State.Class as State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified GHCJS.DOM.EventM as DomEvent
import qualified Language.Javascript.JSaddle as Jsaddle
import qualified Miso as Ms
import qualified Miso.String as Ms
import qualified Text.Read as Text


upd :: IORef MCtx -> Act' -> Ms.Transition Act' Mdl ()
upd mCtxIoRef act
    =
    case act of
        AlsoJsAct (AddTrig (OneSteppedSingTrig (UnitSectorTrigger _ _)))
            -> do
            return ()
        AlsoJsAct (AddTrig trigSteppness)
            -> do
            mbSlctedPlayerIsInsTrigAct <- State.gets
                                              (^. slctedPlayerIsInsTrigAct)
            mbSlctedPose <- State.gets (^. slctedPose)
            trigGrpingIsOn' <- State.gets (^. trigGrpingIsOn)
            case Reaction <$> mbSlctedPlayerIsInsTrigAct <*> mbSlctedPose of
                Just slctedReact
                    ->
                    case trigSteppness of
                        OneSteppedSameHSizeUnitSectTrig vSize
                            -> do
                            addGrpedUnitSectTrig slctedReact
                                $ UnitSectorTrigger vSize mempty
                        OneSteppedSameVSizeUnitSectTrig hSize
                            -> do
                            addGrpedUnitSectTrig slctedReact
                                $ UnitSectorTrigger mempty hSize
                        OneSteppedSingTrig singTrig
                            -> do
                            addTrig singTrig slctedReact trigGrpingIsOn'
                        XSteppedUnitSectTrig (Just vSize, Nothing)
                            -> do
                            addTrig
                                (UnitSectorTrigger vSize mempty)
                                slctedReact
                                trigGrpingIsOn'
                            slctedPrefBodyEdAct
                                .= AddTrig
                                       (XSteppedUnitSectTrig
                                            (Nothing, Just mempty)
                                       )
                        XSteppedUnitSectTrig (Nothing, Just hSize)
                            -> do
                            slctedPref
                                . _LifefulPrefab
                                . identity
                                . body
                                . at slctedReact
                                . _Just
                                . _last
                                . _AnySingleTrigger
                                . _UnitSectorTrigger
                                . _2
                                .= hSize
                            slctedPref
                                . _StaticPrefab
                                . body
                                . at slctedReact
                                . _Just
                                . _last
                                . _AnySingleTrigger
                                . _UnitSectorTrigger
                                . _2
                                .= hSize
                            slctedPrefBodyEdAct
                                .= AddTrig
                                       (XSteppedUnitSectTrig
                                            (Just mempty, mempty)
                                       )
                        _
                            -> do
                            return ()
                Nothing
                    ->
                    return ()
            newMdl <- State.get
            Ms.scheduleIO $ do
                liftIO $ modifyIORef mCtxIoRef (& mdl .~ newMdl)
                return $ OnlySimpAct Loop
        AlsoJsAct (AddTrigGrp trigGrpComm)
            -> do
            mbSlctedPlayerIsInsTrigAct <- State.gets
                                              (^. slctedPlayerIsInsTrigAct)
            mbSlctedPose <- State.gets (^. slctedPose)
            mbSlctedSingTrig <- State.gets (^. slctedSingTrig)
            case
                (,)
                    <$> (Reaction
                             <$> mbSlctedPlayerIsInsTrigAct
                             <*> mbSlctedPose
                        )
                    <*> mbSlctedSingTrig
                of
                Just (slctedReact, slctedSingTrig')
                    -> do
                    slctedPref
                        . _LifefulPrefab
                        . identity
                        . body
                        . at slctedReact
                        . non []
                        %= (List.nub
                            . (++ [ AnyTriggerCriterionGroup
                                        $ initialize trigGrpComm
                                  ]
                              )
                           )
                    slctedPref
                        . _StaticPrefab
                        . body
                        . at slctedReact
                        . non []
                        %= (List.nub
                            . (++ [ AnyTriggerCriterionGroup
                                        $ initialize trigGrpComm
                                  ]
                              )
                           )
                    slctedPrefBodyEdAct
                        .= AddTrig
                               (case (slctedSingTrig', trigGrpComm) of
                                    (  UnitSectorTrigger _ hSize
                                     , SameHorizontalSizeGroupCommon _
                                     )
                                        ->
                                        OneSteppedSameHSizeUnitSectTrig hSize
                                    (  UnitSectorTrigger vSize _
                                     , SameVerticalSizeGroupCommon _
                                     )
                                        ->
                                        OneSteppedSameVSizeUnitSectTrig vSize
                                    _
                                        ->
                                        OneSteppedSingTrig slctedSingTrig'
                               )
                    trigGrpingIsOn .= True
                Nothing
                    -> do
                    return () 
            newMdl <- State.get
            Ms.scheduleIO $ do
                liftIO $ modifyIORef mCtxIoRef (& mdl .~ newMdl)
                return $ OnlySimpAct Loop
        AlsoJsAct View
            -> do
            return ()
        OnlySimpAct (AltSlctedLiflPrefRunSpd msRunSpd)
            -> do
            case Ms.fromMisoStringEither msRunSpd
                 :: Ms.FromMisoString a
                 => Either String a
                of
                Left _
                    ->
                    return ()
                Right runSpd
                    ->
                    slctedPref . _LifefulPrefab . properties . Lens.runSpeed
                        .= Pref.runSpeed runSpd
        OnlySimpAct (ChngPrefName "")
            -> do
            return ()
        OnlySimpAct (ChngPrefName newPrefName)
            -> do
            Ms.scheduleIO $ do
                mCtx <- liftIO $ readIORef mCtxIoRef
                gltfLoadSucCb <- toJSVal . Jsaddle.function $ \_ _ [gltf] -> do
                                     gltfScn <- Three.scn' gltf
                                     Three.add (mCtx ^. scn) gltfScn
                gltfLoadProcCb <- toJSVal . Jsaddle.function
                                      $ \_ _ _ -> return ()
                gltfLoadFailCb <- toJSVal . Jsaddle.function
                                      $ \_ _ _ -> return ()
                gltfLoader <- Three.gltfLoader
                Three.loadGltf gltfLoader
                    ("3d-mdl/" <> Ms.ms newPrefName <> ".glb")
                    gltfLoadSucCb
                    gltfLoadProcCb
                    gltfLoadFailCb
                return $ OnlySimpAct Loop
            slctedPref . _LifefulPrefab . identity . name .= newPrefName
            slctedPref . _StaticPrefab . name .= newPrefName
        OnlySimpAct (DelPref msPrefName)
            -> do
            Ms.scheduleIO . liftIO $ do
                case Ms.fromMisoStringEither msPrefName of
                    Left _
                        -> do
                        return ()
                    Right prefName
                        -> do
                        prefFileCont <- readFile "data/auto/Pref.hs"
                        removeFile "data/auto/Pref.hs"
                        writeFile "data/auto/Pref.hs"
                            . delSlctedPref prefName
                            . exclSlctedPref prefName
                            $ prefFileCont
                return $ OnlySimpAct Loop
        OnlySimpAct (DelTrig trigIx)
            -> do
            mbSlctedPlayerIsInsTrigAct <- State.gets
                                              (^. slctedPlayerIsInsTrigAct)
            mbSlctedPose <- State.gets (^. slctedPose)
            case (,) <$> mbSlctedPlayerIsInsTrigAct <*> mbSlctedPose of
                Just (slctedPlayerIsInsTrigAct', slctedPose')
                    -> do
                    slctedPref
                        . _LifefulPrefab
                        . identity
                        . body
                        . at (Reaction slctedPlayerIsInsTrigAct' slctedPose')
                        . _Just
                        %= (^.. folded
                            . ifiltered (\ix' _ -> ix' /= fromIntegral trigIx)
                           )
                    slctedPref
                        . _LifefulPrefab
                        . identity
                        . body
                        . at (Reaction slctedPlayerIsInsTrigAct' slctedPose')
                        . non []
                        %= (reverse . drop 2 . reverse)
                    slctedPref
                        . _StaticPrefab
                        . body
                        . at (Reaction slctedPlayerIsInsTrigAct' slctedPose')
                        . _Just
                        %= (^.. folded
                            . ifiltered (\ix' _ -> ix' /= fromIntegral trigIx)
                           )
                    slctedPref
                        . _StaticPrefab
                        . body
                        . at (Reaction slctedPlayerIsInsTrigAct' slctedPose')
                        . non []
                        %= (reverse . drop 2 . reverse)
                    Ms.scheduleIO $ do
                        mCtx <- liftIO $ readIORef mCtxIoRef
                        trigGzmsGrp <- Three.getObjByName
                                           (mCtx ^. scn)
                                           "trigGzms"
                        trigGzms <- Three.children trigGzmsGrp
                        toDelTrigGzm <- trigGzms Jsaddle.!! fromIntegral trigIx
                        Three.rmFromPrnt toDelTrigGzm
                        mbLastTrigGzm <- JsApi.last trigGzms
                        Three.rmFromPrnt $ fromJust mbLastTrigGzm
                        mbLastTrigGzm1 <- JsApi.last trigGzms
                        Three.rmFromPrnt $ fromJust mbLastTrigGzm1
                        return $ OnlySimpAct Loop
                Nothing
                    -> do
                    return ()
        OnlySimpAct Init
            -> do
            Ms.scheduleIO $ do
                mbDoc <- currentDocument
                case mbDoc of
                    Just doc
                        -> do
                        mbCnv <- querySelector doc ("#canv" :: String)
                        case mbCnv of
                            Just canv
                                -> do
                                mbSlctedPrefName <- querySelector
                                                        doc
                                                        ("#slctedPrefName"
                                                        :: String
                                                        )
                                case mbSlctedPrefName of
                                    Just slctedPrefName
                                        ->
                                        setAttribute
                                            slctedPrefName
                                            ("spellcheck" :: String)
                                            ("false" :: String)
                                    Nothing
                                        ->
                                        return ()
                                win <- Window <$> JsApi.win >>= getWindow
                                canvH <- (cfg ^. canvH') <$> getInnerHeight win
                                canvW <- fromIntegral <$> getInnerWidth win
                                cam <- Three.prspCam 90 (canvW / canvH) 0.1 1000
                                camPos <- Three.pos cam
                                Three.setZ camPos $ -10
                                rndrer <- Three.webGlRndrer
                                          =<< toJSVal
                                                  (JsApi.Prms [("canvas", canv)]
                                                  )
                                Three.setSize rndrer canvW canvH
                                ambLight <- Three.ambLight Jsaddle.jsNull
                                axesHlpr <- Three.axesHlpr
                                                $ cfg
                                                      ^. prefEdBodyEditingSqrSideLen
                                                      . to fromIntegral
                                                `div` 2
                                gridHlpr' <- Three.gridHlpr
                                                 (cfg
                                                      ^. prefEdBodyEditingSqrSideLen
                                                      . to fromIntegral
                                                 )
                                                 (cfg
                                                      ^. prefEdBodyEditingSqrSideLen
                                                      . to fromIntegral
                                                 )
                                grndGeom <- Three.plnGeom
                                                (cfg
                                                     ^. prefEdBodyEditingSqrSideLen
                                                     . to fromIntegral
                                                )
                                                $ cfg
                                                      ^. prefEdBodyEditingSqrSideLen
                                                      . to fromIntegral
                                grndMat <- Three.meshBscMat
                                           =<< toJSVal
                                                   (JsApi.Prms
                                                       [ ( "opacity"
                                                         , toJSVal (0 :: Int)
                                                         )
                                                       , ( "transparent"
                                                         , toJSVal True
                                                         )
                                                       ]
                                                   )
                                grnd <- Three.mesh grndGeom grndMat
                                grndPos <- Three.pos grnd
                                Three.setY grndPos $ -0.001
                                cellTrigGrndRot <- Three.rot grnd
                                Three.setX cellTrigGrndRot $ -tau / 4
                                slctedEphemPrefWGzmGrp <- Three.grp
                                trigGzmGrp <- Three.grp 
                                Three.setName trigGzmGrp "trigGzms"
                                scn' <- Three.scn
                                forM_
                                    [ axesHlpr
                                    , gridHlpr'
                                    , grnd
                                    , ambLight
                                    , slctedEphemPrefWGzmGrp
                                    , trigGzmGrp
                                    ]
                                    $ Three.add scn'
                                liftIO $ modifyIORef mCtxIoRef (& scn .~ scn')
                                liftIO
                                    $ modifyIORef
                                          mCtxIoRef
                                          (& anim .~ Three.rndr rndrer scn' cam)
                                rndrerDomElem <- Three.domElem rndrer
                                orbCtrls <- Three.orbCtrls cam rndrerDomElem
                                pntr <- Three.vec2 0 0
                                raycstr <- Three.raycstr
                                _ <- DomEvent.on
                                         canv
                                         (EventNameSyncDefault "contextmenu"
                                         :: EventName Element MouseEvent
                                         )
                                         $ do
                                             DomEvent.preventDefault
                                             liftJSM $ do
                                                 orbCtrlsEnabled <- Three.enabled
                                                                        orbCtrls
                                                 Three.setEnabled orbCtrls
                                                     $ not orbCtrlsEnabled
                                _ <- DomEvent.on
                                         canv
                                         (EventNameAsyncDefault "dblclick"
                                         :: EventName Element MouseEvent
                                         )
                                         . liftJSM $ do
                                               trigGzms <- Three.children
                                                               trigGzmGrp
                                               xs <- Three.intersectObjs raycstr
                                                         trigGzms
                                               mbX <- JsApi.head xs
                                               case mbX of
                                                   Just x1
                                                       -> do
                                                       xedTrigGzm <- Three.obj
                                                                         x1
                                                       xedTrigGzmPrnt <- Three.prnt
                                                                             xedTrigGzm
                                                       xedTrigGzmPrntName <- Three.name
                                                                                 xedTrigGzmPrnt
                                                       trigIx <- JsApi.ixOf
                                                                     trigGzms
                                                           $ if xedTrigGzmPrntName
                                                             == "trigGzms"
                                                             then xedTrigGzm
                                                             else xedTrigGzmPrnt
                                                       liftIO
                                                           $ modifyIORef
                                                                 mCtxIoRef
                                                                 (& animAct
                                                                  .~ OnlySimpAct
                                                                         (DelTrig
                                                                             (fromIntegral
                                                                                  trigIx
                                                                             )
                                                                         )
                                                                 )
                                                   Nothing
                                                      -> do
                                                      return ()
                                _ <- DomEvent.on
                                    canv
                                    (EventNameAsyncDefault "pointerdown"
                                     :: EventName Element MouseEvent
                                    )
                                    $ do
                                        btn <- DomEvent.mouseButton
                                        mCtx <- liftIO $ readIORef mCtxIoRef
                                        orbCtrlsEnabled <- liftJSM
                                                               $ Three.enabled
                                                                     orbCtrls
                                        if btn == 0 && not orbCtrlsEnabled
                                        then do
                                            (pntrX, pntrY) <- DomEvent.mouseXY
                                            liftJSM $ do
                                                Three.setX pntr
                                                    $ fromIntegral pntrX
                                                    / canvW
                                                    * 2
                                                    - 1
                                                Three.setZ pntr
                                                    $ fromIntegral (-pntrY)
                                                    / canvH
                                                    * 2
                                                    + 1
                                                Three.setRaycstrFromCam raycstr
                                                    pntr
                                                    cam
                                                xs <- Three.intersectObj raycstr
                                                          grnd
                                                mbX <- JsApi.head xs
                                                case mbX of
                                                    Just x
                                                        -> do
                                                        xPnt <- Three.pnt x
                                                        xPntX <- Three.x xPnt
                                                        xPntZ <- Three.z xPnt
                                                        case
                                                            mCtx
                                                                ^. mdl
                                                                . slctedPrefBodyEdAct
                                                            of
                                                            AddTrig
                                                                (OneSteppedSameHSizeUnitSectTrig
                                                                     _
                                                                )
                                                                -> do
                                                                case mCtx
                                                                         ^. mdl
                                                                         . slctedPlayerIsInsTrigAct
                                                                    of
                                                                    Just slctedPrefBodyEdAct'
                                                                        -> do
                                                                        dotProdTrigGzm <- drawSect
                                                                                              (cfg
                                                                                               ^. trigGzmClrs
                                                                                               . at
                                                                                                     slctedPrefBodyEdAct'
                                                                                               . non
                                                                                                     ""
                                                                                              )
                                                                                              trigGzmGrp
                                                                                              xPntZ
                                                                        dotProdTrigGzmPos <- Three.pos
                                                                                                 dotProdTrigGzm
                                                                        Three.setX
                                                                            dotProdTrigGzmPos
                                                                            $ cfg
                                                                            ^. prefEdBodyEditingSqrSideLen
                                                                            . to
                                                                                  fromIntegral
                                                                            / 2
                                                                        dotProdTrigGzmRot <- Three.rot
                                                                                                 dotProdTrigGzm
                                                                        Three.setZ
                                                                            dotProdTrigGzmRot
                                                                            $ -tau
                                                                            / 4
                                                                        liftIO
                                                                            $ modifyIORef
                                                                                  mCtxIoRef
                                                                                  (& animAct
                                                                                   .~ AlsoJsAct
                                                                                          (AddTrig
                                                                                              (OneSteppedSameHSizeUnitSectTrig
                                                                                                   (unitDotProduct
                                                                                                        ( abs
                                                                                                              xPntZ
                                                                                                        / ( cfg
                                                                                                            ^. prefEdBodyEditingSqrSideLen
                                                                                                            . to
                                                                                                                  fromIntegral
                                                                                                          / 2
                                                                                                          )
                                                                                                        )
                                                                                                   )
                                                                                              )
                                                                                          )
                                                                                  )
                                                                    Nothing
                                                                        -> do
                                                                        return ()
                                                            AddTrig
                                                                (OneSteppedSameVSizeUnitSectTrig
                                                                     _
                                                                )
                                                                -> do
                                                                case
                                                                    mCtx
                                                                        ^. mdl
                                                                        . slctedPlayerIsInsTrigAct
                                                                    of
                                                                    Just slctedPrefBodyEdAct'
                                                                        -> do
                                                                        dotProdTrigGzm <- drawSect
                                                                                              (cfg
                                                                                               ^. trigGzmClrs
                                                                                               . at
                                                                                                     slctedPrefBodyEdAct'
                                                                                               . non
                                                                                                     ""
                                                                                              )
                                                                                              trigGzmGrp
                                                                                              xPntX
                                                                        dotProdTrigGzmPos <- Three.pos
                                                                                                 dotProdTrigGzm
                                                                        Three.setZ
                                                                            dotProdTrigGzmPos
                                                                            $ cfg
                                                                            ^. prefEdBodyEditingSqrSideLen
                                                                            . to
                                                                                  fromIntegral
                                                                            / (-2)
                                                                        liftIO
                                                                            $ modifyIORef
                                                                                  mCtxIoRef
                                                                                  (& animAct
                                                                                   .~ AlsoJsAct
                                                                                          (AddTrig
                                                                                              (OneSteppedSameVSizeUnitSectTrig
                                                                                                   (unitDotProduct
                                                                                                        ( abs
                                                                                                              xPntX
                                                                                                        / ( cfg
                                                                                                            ^. prefEdBodyEditingSqrSideLen
                                                                                                            . to
                                                                                                                  fromIntegral
                                                                                                          / 2
                                                                                                          )
                                                                                                        )
                                                                                                   )
                                                                                              )
                                                                                          )
                                                                                  )
                                                                    Nothing
                                                                        -> do
                                                                        return ()
                                                            AddTrig
                                                                (OneSteppedSingTrig
                                                                     (CellRectangleTrigger
                                                                          _
                                                                          _
                                                                     )
                                                                )
                                                                ->
                                                                case mCtx
                                                                         ^. mdl
                                                                         . slctedPlayerIsInsTrigAct
                                                                    of
                                                                    Just slctedPrefBodyEdAct'
                                                                        -> do
                                                                        let
                                                                            cell = V.Vector2
                                                                                       cellClm
                                                                                       cellRow
                                                                            cellClm = round
                                                                                          xPntX
                                                                            cellRow = round
                                                                                          xPntZ
                                                                        cellTrigGeom <- Three.plnGeom
                                                                                            1
                                                                                            1
                                                                        cellTrigMat <- gzmMat
                                                                                           $ cfg
                                                                                           ^. trigGzmClrs
                                                                                           . at
                                                                                                 slctedPrefBodyEdAct'
                                                                                           . non ""
                                                                                           . to toJSVal
                                                                        cellTrigGzm <- Three.mesh
                                                                                           cellTrigGeom
                                                                                           cellTrigMat
                                                                        cellTrigGzmOrg <- Three.grp
                                                                        cellTrigGzmOrgScl <- Three.scl
                                                                                                 cellTrigGzmOrg
                                                                        Three.setX
                                                                            cellTrigGzmOrgScl
                                                                            2
                                                                        Three.setZ
                                                                            cellTrigGzmOrgScl
                                                                            2
                                                                        cellTrigGzmScl <- Three.scl
                                                                                              cellTrigGzm
                                                                        Three.setX
                                                                            cellTrigGzmScl
                                                                            0.5
                                                                        Three.setY
                                                                            cellTrigGzmScl
                                                                            0.5
                                                                        cellTrigGzmPos <- Three.pos
                                                                                              cellTrigGzm
                                                                        Three.setX
                                                                            cellTrigGzmPos
                                                                            $ -0.25
                                                                        Three.setZ
                                                                            cellTrigGzmPos
                                                                            $ -0.25
                                                                        Three.add trigGzmGrp
                                                                            cellTrigGzmOrg
                                                                        cellTrigGzmOrgPos <- Three.pos
                                                                                                 cellTrigGzmOrg
                                                                        Three.setX
                                                                            cellTrigGzmOrgPos
                                                                            $ fromIntegral
                                                                                  cellClm
                                                                            + 0.5
                                                                        Three.setZ
                                                                            cellTrigGzmOrgPos
                                                                            $ fromIntegral
                                                                                  cellRow
                                                                            + 0.5
                                                                        cellTrigGzmRot <- Three.rot
                                                                                              cellTrigGzm
                                                                        Three.setX
                                                                            cellTrigGzmRot
                                                                            $ -tau / 4
                                                                        Three.add
                                                                            cellTrigGzmOrg
                                                                            cellTrigGzm
                                                                        liftIO
                                                                            . writeIORef
                                                                                  mCtxIoRef 
                                                                            $ mCtx
                                                                            & adjustedTrigLike
                                                                            .~ Just
                                                                                   (AnySingleTrigger
                                                                                        (CellRectangleTrigger
                                                                                             ((+ 1)
                                                                                              <$> cell
                                                                                             )
                                                                                             cell
                                                                                        )
                                                                                   )
                                                                    Nothing
                                                                        -> do
                                                                        return ()
                                                            AddTrig
                                                                (OneSteppedSingTrig
                                                                     (CircleTrigger
                                                                          _
                                                                          _
                                                                     )
                                                                )
                                                                -> case mCtx
                                                                            ^. mdl
                                                                            . slctedPlayerIsInsTrigAct
                                                                    of
                                                                    Just
                                                                        slctedPrefBodyEdAct'
                                                                        -> do
                                                                        circTrigGzmGeom <- Three.circGeom
                                                                                               0
                                                                                               (cfg
                                                                                                    ^. sectSegQnt
                                                                                                    . to
                                                                                                          fromIntegral
                                                                                               )
                                                                                               0
                                                                                               tau
                                                                        circTrigGzmMat <- gzmMat
                                                                                              $ cfg
                                                                                              ^. trigGzmClrs
                                                                                              . at
                                                                                                    slctedPrefBodyEdAct'
                                                                                              . non ""
                                                                                              . to toJSVal
                                                                        circTrigGzm <- Three.mesh
                                                                                           circTrigGzmGeom
                                                                                           circTrigGzmMat
                                                                        circTrigGzmPos <- Three.pos
                                                                                              circTrigGzm
                                                                        Three.setX
                                                                            circTrigGzmPos
                                                                            xPntX
                                                                        Three.setY
                                                                            circTrigGzmPos
                                                                            0.001
                                                                        Three.setZ
                                                                            circTrigGzmPos
                                                                            xPntZ
                                                                        circTrigGzmRot <- Three.rot
                                                                                              circTrigGzm
                                                                        Three.setX
                                                                            circTrigGzmRot
                                                                            $ -tau / 4
                                                                        Three.add
                                                                            trigGzmGrp
                                                                            circTrigGzm
                                                                        liftIO
                                                                            . writeIORef
                                                                                  mCtxIoRef
                                                                            $ mCtx
                                                                            & adjustedTrigLike
                                                                            .~ Just
                                                                                   (AnySingleTrigger
                                                                                        (CircleTrigger
                                                                                             (V.Vector2
                                                                                                  xPntX
                                                                                                  xPntZ
                                                                                             )
                                                                                             mempty
                                                                                        )
                                                                                   )
                                                                    Nothing
                                                                        -> do
                                                                        return ()
                                                            AddTrig
                                                                (OneSteppedSingTrig
                                                                     (HorizontalUnitSectorTrigger
                                                                          _
                                                                     )
                                                                )
                                                                -> do
                                                                case mCtx
                                                                         ^. mdl
                                                                         . slctedPlayerIsInsTrigAct
                                                                    of
                                                                    Just
                                                                        slctedPrefBodyEdAct'
                                                                        -> do
                                                                        dotProdTrigGzm <- drawSect
                                                                                              (cfg
                                                                                               ^. trigGzmClrs
                                                                                               . at
                                                                                                     slctedPrefBodyEdAct'
                                                                                               . non
                                                                                                     ""
                                                                                              )
                                                                                              trigGzmGrp
                                                                                              xPntX
                                                                        dotProdTrigGzmPos <- Three.pos
                                                                                                 dotProdTrigGzm
                                                                        Three.setZ
                                                                            dotProdTrigGzmPos
                                                                            $ cfg
                                                                            ^. prefEdBodyEditingSqrSideLen
                                                                            . to
                                                                                  ( negate
                                                                                  . fromIntegral
                                                                                  )
                                                                            / 2
                                                                        liftIO
                                                                            $ modifyIORef
                                                                                  mCtxIoRef
                                                                                  (& animAct
                                                                                   .~ AlsoJsAct
                                                                                          (AddTrig
                                                                                              (OneSteppedSingTrig
                                                                                                   (HorizontalUnitSectorTrigger
                                                                                                        (unitDotProduct
                                                                                                             ( abs
                                                                                                                   xPntX
                                                                                                             / ( cfg
                                                                                                                 ^. prefEdBodyEditingSqrSideLen
                                                                                                                 . to
                                                                                                                       fromIntegral
                                                                                                               / 2
                                                                                                               )
                                                                                                             )
                                                                                                        )
                                                                                                   )
                                                                                              )
                                                                                          )
                                                                                  )
                                                                    Nothing
                                                                        -> do
                                                                        return ()
                                                            AddTrig
                                                                (XSteppedUnitSectTrig
                                                                     prtlUnitSectTrig
                                                                )
                                                                -> do
                                                                case mCtx
                                                                         ^. mdl
                                                                         . slctedPlayerIsInsTrigAct
                                                                    of
                                                                    Just
                                                                        slctedPrefBodyEdAct'
                                                                        -> do
                                                                        mCtx' <- liftIO
                                                                                    $ readIORef
                                                                                          mCtxIoRef
                                                                        if mCtx'
                                                                              ^. mdl
                                                                              .  trigGrpingIsOn
                                                                        then do
                                                                            return ()
                                                                        else do
                                                                            case prtlUnitSectTrig of
                                                                                (Just _, Nothing)
                                                                                    -> do
                                                                                    dotProdTrigGzm <- drawSect
                                                                                                          (cfg
                                                                                                           ^. trigGzmClrs
                                                                                                           . at
                                                                                                                 slctedPrefBodyEdAct'
                                                                                                           . non
                                                                                                                 ""
                                                                                                          )
                                                                                                          trigGzmGrp
                                                                                                          xPntZ
                                                                                    dotProdTrigGzmPos <- Three.pos
                                                                                                             dotProdTrigGzm
                                                                                    Three.setX
                                                                                        dotProdTrigGzmPos
                                                                                        $ cfg
                                                                                        ^. prefEdBodyEditingSqrSideLen
                                                                                        . to
                                                                                              fromIntegral
                                                                                        / 2
                                                                                    dotProdTrigGzmRot <- Three.rot
                                                                                                             dotProdTrigGzm
                                                                                    Three.setZ
                                                                                        dotProdTrigGzmRot
                                                                                        $ -tau
                                                                                        / 4
                                                                                    liftIO
                                                                                        $ modifyIORef
                                                                                              mCtxIoRef
                                                                                              (& animAct
                                                                                               .~ AlsoJsAct
                                                                                                      (AddTrig
                                                                                                           (XSteppedUnitSectTrig
                                                                                                                (Just
                                                                                                                     (unitDotProduct
                                                                                                                          ( abs
                                                                                                                                xPntZ
                                                                                                                          / ( cfg
                                                                                                                              ^. prefEdBodyEditingSqrSideLen
                                                                                                                              . to
                                                                                                                                    fromIntegral
                                                                                                                            / 2
                                                                                                                            )
                                                                                                                          )
                                                                                                                     )
                                                                                                                , mempty
                                                                                                                )
                                                                                                           )
                                                                                                      )
                                                                                              )
                                                                                (Nothing, Just _)
                                                                                    -> do
                                                                                    dotProdTrigGzm <- drawSect
                                                                                                          (cfg
                                                                                                           ^. trigGzmClrs
                                                                                                           . at
                                                                                                                 slctedPrefBodyEdAct'
                                                                                                           . non
                                                                                                                 ""
                                                                                                          )
                                                                                                          trigGzmGrp
                                                                                                          xPntX
                                                                                    dotProdTrigGzmPos <- Three.pos
                                                                                                             dotProdTrigGzm
                                                                                    Three.setZ
                                                                                        dotProdTrigGzmPos
                                                                                        $ cfg
                                                                                        ^. prefEdBodyEditingSqrSideLen
                                                                                        . to
                                                                                              fromIntegral
                                                                                        / (-2)
                                                                                    liftIO
                                                                                        $ modifyIORef
                                                                                              mCtxIoRef
                                                                                              (& animAct
                                                                                               .~ AlsoJsAct
                                                                                                      (AddTrig
                                                                                                           (XSteppedUnitSectTrig
                                                                                                                ( mempty
                                                                                                                , Just
                                                                                                                     (unitDotProduct
                                                                                                                          ( abs
                                                                                                                                xPntX
                                                                                                                          / ( cfg
                                                                                                                              ^. prefEdBodyEditingSqrSideLen
                                                                                                                              . to
                                                                                                                                    fromIntegral
                                                                                                                            / 2
                                                                                                                            )
                                                                                                                          )
                                                                                                                     )
                                                                                                                )
                                                                                                           )
                                                                                                      )
                                                                                              )
                                                                                (Just _, Just _)
                                                                                    -> do
                                                                                    return ()
                                                                                (Nothing, Nothing)
                                                                                    -> do
                                                                                    return ()
                                                                    Nothing
                                                                        -> do
                                                                        return ()
                                                            AddTrig _
                                                                -> do
                                                                return ()
                                                            AddTrigGrp
                                                                (SameHorizontalSizeGroupCommon
                                                                     _
                                                                )
                                                                -> do
                                                                case mCtx
                                                                         ^. mdl
                                                                         . slctedPlayerIsInsTrigAct
                                                                    of
                                                                    Just
                                                                        slctedPrefBodyEdAct'
                                                                        -> do
                                                                        dotProdTrigGzm <- drawSect
                                                                                              (cfg
                                                                                               ^. trigGzmClrs
                                                                                               . at
                                                                                                     slctedPrefBodyEdAct'
                                                                                               . non
                                                                                                     ""
                                                                                              )
                                                                                              trigGzmGrp
                                                                                              xPntX
                                                                        dotProdTrigGzmPos <- Three.pos
                                                                                                 dotProdTrigGzm
                                                                        Three.setZ
                                                                            dotProdTrigGzmPos
                                                                            $ cfg
                                                                            ^. prefEdBodyEditingSqrSideLen
                                                                            . to
                                                                                  ( negate
                                                                                  . fromIntegral
                                                                                  )
                                                                            / 2
                                                                        liftIO
                                                                            $ modifyIORef
                                                                                  mCtxIoRef
                                                                                  (&  animAct
                                                                                   .~ AlsoJsAct
                                                                                          (AddTrigGrp
                                                                                               (SameHorizontalSizeGroupCommon
                                                                                                    (unitDotProduct
                                                                                                         ( abs
                                                                                                               xPntX
                                                                                                         / ( cfg
                                                                                                             ^. prefEdBodyEditingSqrSideLen
                                                                                                             . to
                                                                                                                   fromIntegral
                                                                                                           / 2
                                                                                                           )
                                                                                                         )
                                                                                                    )
                                                                                               )
                                                                                          )
                                                                                  )
                                                                    Nothing
                                                                        -> do
                                                                        return ()
                                                            AddTrigGrp
                                                                (SameVerticalSizeGroupCommon
                                                                     _
                                                                )
                                                                ->
                                                                case mCtx
                                                                         ^. mdl
                                                                         . slctedPlayerIsInsTrigAct
                                                                    of
                                                                    Just slctedPrefBodyEdAct'
                                                                        -> do
                                                                        dotProdTrigGzm <- drawSect
                                                                                              (cfg
                                                                                               ^. trigGzmClrs
                                                                                               . at
                                                                                                     slctedPrefBodyEdAct'
                                                                                               . non
                                                                                                     ""
                                                                                              )
                                                                                              trigGzmGrp
                                                                                              xPntZ
                                                                        dotProdTrigGzmPos <- Three.pos
                                                                                                 dotProdTrigGzm
                                                                        Three.setX
                                                                            dotProdTrigGzmPos
                                                                            $ cfg
                                                                            ^. prefEdBodyEditingSqrSideLen
                                                                            . to
                                                                                  fromIntegral
                                                                            / 2
                                                                        dotProdTrigGzmRot <- Three.rot
                                                                                                 dotProdTrigGzm
                                                                        Three.setZ
                                                                            dotProdTrigGzmRot
                                                                            $ -tau
                                                                            / 4
                                                                        liftIO
                                                                            $ modifyIORef
                                                                                  mCtxIoRef
                                                                                  (&  animAct
                                                                                   .~ AlsoJsAct
                                                                                          (AddTrigGrp
                                                                                               (SameVerticalSizeGroupCommon
                                                                                                    (unitDotProduct
                                                                                                         ( abs
                                                                                                               xPntZ
                                                                                                         / ( cfg
                                                                                                             ^. prefEdBodyEditingSqrSideLen
                                                                                                             . to
                                                                                                                   fromIntegral
                                                                                                           / 2
                                                                                                           )
                                                                                                         )
                                                                                                    )
                                                                                               )
                                                                                          )
                                                                                  )
                                                                    Nothing
                                                                        -> do
                                                                        return ()
                                                            View
                                                                -> do
                                                                return ()
                                                    Nothing
                                                        -> do
                                                        return ()
                                            else do
                                                return ()
                                _ <- DomEvent.on
                                         canv
                                         (EventNameAsyncDefault "pointermove"
                                         :: EventName Element MouseEvent
                                         )
                                         $ do
                                         mCtx <- liftIO $ readIORef mCtxIoRef
                                         (mouseX', mouseY') <- DomEvent.mouseXY
                                         liftJSM $ do
                                             Three.setX pntr
                                                $ fromIntegral mouseX'
                                                / canvW
                                                * 2
                                                - 1
                                             Three.setY pntr
                                                $ fromIntegral (-mouseY')
                                                / canvH
                                                * 2
                                                + 1
                                             Three.setRaycstrFromCam
                                                 raycstr
                                                 pntr
                                                 cam
                                             xs <- Three.intersectObj raycstr
                                                       grnd
                                             mbIntersect <- JsApi.head xs
                                             case mbIntersect of
                                                 Just x -> do
                                                     xPnt <- Three.pnt x
                                                     xPntX <- Three.x xPnt
                                                     xPntZ <- Three.z xPnt
                                                     trigGzms <- Three.children
                                                                     trigGzmGrp
                                                     mbLastTrigGzmOrg <- JsApi.last
                                                                             trigGzms
                                                     case mbLastTrigGzmOrg of
                                                         Just lastTrigGzmOrg
                                                             ->
                                                             case mCtx
                                                                  ^. adjustedTrigLike
                                                                 of
                                                                 Just
                                                                     (AnySingleTrigger
                                                                          (CellRectangleTrigger
                                                                               sCell@(V.Vector2
                                                                                        sRow
                                                                                        sClm
                                                                                   )
                                                                               _
                                                                          )
                                                                     )
                                                                     -> do
                                                                     let
                                                                         pntrClm = round
                                                                                       xPntX
                                                                         pntrRow = round
                                                                                       xPntZ
                                                                         cellTrigH = sRow
                                                                                   - 1
                                                                                   - pntrRow
                                                                         cellTrigW = sClm
                                                                                   - 1
                                                                                   - pntrClm
                                                                         cellTrigH' = adjCellTrigSideLen
                                                                                          cellTrigH
                                                                         cellTrigW' = adjCellTrigSideLen
                                                                                          cellTrigW
                                                                     lastTrigGzmOrgScl <- Three.scl
                                                                                              lastTrigGzmOrg
                                                                     Three.setX lastTrigGzmOrgScl
                                                                         $ 2
                                                                         * fromIntegral
                                                                               cellTrigW'
                                                                     Three.setZ lastTrigGzmOrgScl
                                                                         $ 2
                                                                         * fromIntegral
                                                                               cellTrigH'
                                                                     liftIO
                                                                         . writeIORef
                                                                               mCtxIoRef
                                                                         $ mCtx
                                                                         & adjustedTrigLike
                                                                         .~ Just
                                                                                (AnySingleTrigger
                                                                                     (CellRectangleTrigger
                                                                                          sCell
                                                                                          (V.Vector2
                                                                                               (-cellTrigH'
                                                                                               )
                                                                                               (-cellTrigW'
                                                                                               )
                                                                                          )
                                                                                     )
                                                                                )
                                                                 Just
                                                                     (AnySingleTrigger
                                                                          (CircleTrigger
                                                                               circTrigPos
                                                                               (Radius r)
                                                                          )
                                                                     )
                                                                     -> do
                                                                     let
                                                                         circTrigToPntrDist = round
                                                                                            . V.distance
                                                                                                  circTrigPos
                                                                                            $ V.Vector2 
                                                                                                  xPntX
                                                                                                  xPntZ
                                                                                            :: Int
                                                                         lastCircTrigGzm = lastTrigGzmOrg
                                                                     newCircTrigGzmGeom <- Three.circGeom
                                                                                               r 
                                                                                               ( (cfg
                                                                                                      ^. sectSegQnt
                                                                                                      . to
                                                                                                            fromIntegral
                                                                                                 )
                                                                                               * circTrigToPntrDist
                                                                                               )
                                                                                               0
                                                                                               tau
                                                                     lastCircTrigGzmGeom <- Three.geom
                                                                                                lastCircTrigGzm
                                                                     Three.copy
                                                                         lastCircTrigGzmGeom
                                                                         newCircTrigGzmGeom
                                                                     liftIO
                                                                         . writeIORef
                                                                               mCtxIoRef
                                                                         $ mCtx
                                                                         & adjustedTrigLike 
                                                                         .~ Just
                                                                                (AnySingleTrigger
                                                                                     (CircleTrigger
                                                                                          circTrigPos
                                                                                          (Radius
                                                                                               (fromIntegral
                                                                                                    circTrigToPntrDist
                                                                                               )
                                                                                          )
                                                                                     )
                                                                                )
                                                                 _
                                                                    -> do
                                                                    return ()
                                                         Nothing
                                                            -> do
                                                            return ()
                                                 Nothing
                                                    -> do
                                                    return ()
                                _ <- DomEvent.on
                                         canv
                                         (EventNameAsyncDefault "pointerup"
                                          :: EventName Element MouseEvent
                                         )
                                     . liftIO
                                     . modifyIORef mCtxIoRef 
                                     $ \mCtx
                                           ->
                                           case mCtx ^. adjustedTrigLike of
                                               Just adjustedTrigLike'
                                                   ->
                                                   case adjustedTrigLike' of
                                                       AnyTriggerCriterionGroup
                                                           (SameHorizontalSizeTriggerGroup
                                                                _
                                                                _
                                                           )
                                                           ->
                                                           mCtx
                                                       AnyTriggerCriterionGroup
                                                           (SameVerticalSizeTriggerGroup
                                                                _
                                                                _
                                                           )
                                                           ->
                                                           mCtx
                                                       AnySingleTrigger
                                                           adjustedSingTrig
                                                           ->
                                                           mCtx
                                                               & adjustedTrigLike
                                                                 .~ Nothing
                                                               & animAct
                                                                 .~ AlsoJsAct
                                                                        (AddTrig
                                                                             (OneSteppedSingTrig
                                                                                  adjustedSingTrig
                                                                             )
                                                                        )
                                               Nothing
                                                   ->
                                                   mCtx
                                return ()
                            Nothing
                                -> do
                                return ()
                    Nothing
                        -> do
                        return ()
                return $ OnlySimpAct Loop
        OnlySimpAct Loop
            -> do
            Ms.scheduleIO . nextAnimationFrame $ \_ -> do
                mCtx <- liftIO $ do
                    threadDelay $ 1000 `div` 60 * 1000
                    mCtx <- readIORef mCtxIoRef
                    writeIORef mCtxIoRef $ mCtx & animAct .~ OnlySimpAct Loop
                    return mCtx
                mCtx ^. anim
                return $ mCtx ^. animAct
        OnlySimpAct RlsTrigGrp
            -> do
            mbSlctedSingTrig <- State.gets (^. slctedSingTrig)
            trigGrpingIsOn .= False
            case mbSlctedSingTrig of
                Just slctedSingTrig'
                    ->
                    slctedPrefBodyEdAct
                        .= AddTrig
                               (case slctedSingTrig' of
                                    UnitSectorTrigger _ _
                                        ->
                                        XSteppedUnitSectTrig
                                            (Just mempty, mempty)
                                    singTrig
                                        ->
                                        OneSteppedSingTrig singTrig
                               )
                Nothing
                    ->
                    return ()
        OnlySimpAct SavePref
            -> do
            slctedPref' <- State.gets (^. slctedPref)
            case viewPrefName slctedPref' of
                Just slctedPrefName
                    ->
                    Ms.scheduleIO . liftIO $ do
                        prefFileCont <- readFile "data/auto/Pref.hs"
                        removeFile "data/auto/Pref.hs"
                        writeFile "data/auto/Pref.hs"
                            . ((unlines . take 10 . lines) prefFileCont ++)
                            . ("prefs=" ++)
                            . (++ " :: Map String (Prefab String Double Int)")
                            . show
                            $ Map.insert slctedPrefName slctedPref' prefs
                        return $ OnlySimpAct (SlctPref (Ms.ms slctedPrefName))
                Nothing
                    ->
                    return ()
        OnlySimpAct (SlctPref newSlctedPrefName)
            -> do
            case
                filterM
                    ( liftM2 (==) (Just newSlctedPrefName)
                    . fmap Ms.ms
                    . viewPrefName
                    )
                    $ Map.elems prefs
                of
                Just [newSlctedPref]
                    -> do
                    slctedPlayerInsTrigAct <- State.gets
                                                  (^. slctedPlayerIsInsTrigAct)
                    mbSlctedPose <- State.gets (^. slctedPose)
                    slctedPref .= newSlctedPref
                    slctedPrefCopy .= newSlctedPref
                    Ms.scheduleIO $ do
                        drawTrigGzms
                            slctedPlayerInsTrigAct
                            mbSlctedPose
                            newSlctedPref
                        case viewPrefName newSlctedPref of
                            Just slctedPrefName
                                -> do
                                mCtx <- liftIO $ readIORef mCtxIoRef
                                gltfLoadSucCb <- toJSVal . Jsaddle.function
                                                     $ \_ _ [gltf] -> do
                                                           gltfScn <- Three.scn'
                                                                          gltf
                                                           Three.add
                                                               (mCtx ^. scn)
                                                               gltfScn
                                gltfLoadProcCb <- toJSVal . Jsaddle.function
                                                      $ \_ _ _ -> return ()
                                gltfLoadFailCb <- toJSVal . Jsaddle.function
                                                      $ \_ _ _ -> return ()
                                gltfLoader <- Three.gltfLoader
                                Three.loadGltf gltfLoader
                                    ("3d-mdl/" <> Ms.ms slctedPrefName <> ".glb"
                                    )
                                    gltfLoadSucCb
                                    gltfLoadProcCb
                                    gltfLoadFailCb
                            Nothing
                                -> do
                                return ()
                        return $ OnlySimpAct Loop
                Just _
                    -> do
                    return ()
                Nothing
                    -> do
                    return ()
        OnlySimpAct (SlctPrefType strNewSlctedPref0)
            -> do
            slctedPref' <- State.gets (^. slctedPref)
            case Ms.fromMisoStringEither strNewSlctedPref0 >>= Text.readEither
                of
                Left _ -> return ()
                Right newSlctedPref0 ->
                    slctedPref .= Pref.transform slctedPref' newSlctedPref0
        OnlySimpAct (UnsafeSlctHumanPose msNewSlctedPose)
            -> do
            case Ms.fromMisoStringEither msNewSlctedPose >>= Text.readEither
                of
                Left _ -> return ()
                Right newSlctedPose -> do
                    newMdl <- State.get
                    slctedPlayerInsTrigAct <- State.gets
                                                  (^. slctedPlayerIsInsTrigAct)
                    slctedPose .= Just newSlctedPose
                    slctedPref' <- State.gets (^. slctedPref)
                    Ms.scheduleIO_ $ do
                        liftIO $ modifyIORef mCtxIoRef (& mdl .~ newMdl)
                        drawTrigGzms
                            slctedPlayerInsTrigAct
                            (Just newSlctedPose)
                            slctedPref'
        OnlySimpAct (UnsafeSlctPrefBodyEdAct msNewSlctedPrefDodyEdAct)
            -> do
            case Text.readEither
                 =<< Ms.fromMisoStringEither msNewSlctedPrefDodyEdAct
                of
                Left _
                    -> do
                    return ()
                Right newSlctedPrefDodyEdAct
                    -> do
                    slctedPrefBodyEdAct .= newSlctedPrefDodyEdAct
                    newMdl <- State.get
                    Ms.scheduleIO . liftIO $ do
                        modifyIORef mCtxIoRef (& mdl .~ newMdl)
                        return $ OnlySimpAct Loop
        OnlySimpAct (UnsafeSlctSingTrig msNewSlctedSingTrig)
            -> do
            case Text.readEither =<< Ms.fromMisoStringEither msNewSlctedSingTrig
                of
                Left _
                    -> do
                    return ()
                Right newSlctedSingTrig
                    -> do
                    slctedPrefBodyEdAct
                        .= AddTrig
                               (case newSlctedSingTrig of
                                    UnitSectorTrigger _ _
                                        ->
                                        XSteppedUnitSectTrig
                                            (Just mempty, Nothing)
                                    singTrig
                                        ->
                                        OneSteppedSingTrig singTrig
                               )
                    slctedSingTrig .= Just newSlctedSingTrig
                    newMdl <- State.get
                    Ms.scheduleIO . liftIO $ do
                        modifyIORef mCtxIoRef (& mdl .~ newMdl)
                        return $ OnlySimpAct Loop
        OnlySimpAct (UnsafeSlctXAct msNewSlctedPlayerInsTrigAct)
            -> do
            mbSlctedPose <- State.gets (^. slctedPose)
            newMdl <- State.get
            slctedPref' <- State.gets (^. slctedPref)
            case Ms.fromMisoStringEither msNewSlctedPlayerInsTrigAct
                >>= Text.readEither
                of
                Left _
                    -> do
                    return ()
                Right newSlctedPlayerInsTrigAct
                    -> do
                    slctedPlayerIsInsTrigAct .= Just newSlctedPlayerInsTrigAct
                    Ms.scheduleIO_ $ do
                        liftIO $ modifyIORef mCtxIoRef (& mdl .~ newMdl)
                        drawTrigGzms
                            (Just newSlctedPlayerInsTrigAct)
                            mbSlctedPose
                            slctedPref'
  where
    addGrpedUnitSectTrig slctedReact unitSectTrig@(UnitSectorTrigger _ _)
        =
        slctedPref . _StaticPrefab . body . at slctedReact . _Just . _last
            %= (\(AnyTriggerCriterionGroup trigGrp)
                    ->
                    case tryToAdd unitSectTrig trigGrp of
                        NonGroupableSingleTrigger
                            ->
                            AnyTriggerCriterionGroup trigGrp
                        TriggerCriterionGroup trigGrp'
                            ->
                            AnyTriggerCriterionGroup trigGrp'
               )
    addGrpedUnitSectTrig _ _
        =
        return ()
    addTrig singTrig slctedReact trigGrpCritTyIsSlcted'
        =
        if trigGrpCritTyIsSlcted'
        then do
            slctedPref
                . _LifefulPrefab
                . identity
                . body
                . at slctedReact
                . _Just
                . _last
                %= (\(AnyTriggerCriterionGroup trigGrp) ->
                        case tryToAdd singTrig trigGrp of
                            NonGroupableSingleTrigger
                                ->
                                AnyTriggerCriterionGroup trigGrp
                            TriggerCriterionGroup trigGrp'
                                ->
                                AnyTriggerCriterionGroup trigGrp'
                   )
            slctedPref
                . _StaticPrefab
                . body
                . at slctedReact
                . _Just
                . _last
                %= (\(AnyTriggerCriterionGroup trigGrp) ->
                        case tryToAdd singTrig trigGrp of
                            NonGroupableSingleTrigger
                                ->
                                AnyTriggerCriterionGroup trigGrp
                            TriggerCriterionGroup trigGrp'
                                ->
                                AnyTriggerCriterionGroup trigGrp'
                   )
        else do
            slctedPref
                . _LifefulPrefab
                . identity
                . body
                . at slctedReact
                . non []
                %= (List.nub
                    . (++ [AnySingleTrigger $ trim singTrig])
                   )
            slctedPref
                .  _StaticPrefab
                .  body
                .  at slctedReact
                .  non []
                %= (List.nub
                    . (++ [AnySingleTrigger $ trim singTrig])
                   )
    adjCellTrigSideLen 0 = 1
    adjCellTrigSideLen len | len > 0 = len + 1
    adjCellTrigSideLen len = len
    adjTrigGzmOrg gzmOrg posX posZ sclX sclZ = do
        gzmOrgPos <- Three.pos gzmOrg
        Three.setX gzmOrgPos $ fromIntegral posX - 0.5
        Three.setY gzmOrgPos 0.5
        Three.setZ gzmOrgPos $ fromIntegral posZ - 0.5
        gzmOrgScl <- Three.scl gzmOrg
        Three.setX gzmOrgScl $ fromIntegral sclX
        Three.setZ gzmOrgScl $ fromIntegral sclZ
    delSlctedPref prefName'
        =
        unlines . filter (not . List.isPrefixOf (prefName' ++ "=")) . lines
    drawSect sectClr sectPrnt dotProd = do
        sectGeom <- Three.circGeom
                        ( cfg
                          ^. prefEdBodyEditingSqrSideLen
                          . to fromIntegral
                        / 2
                        )
                        ( cfg ^. sectSegQnt . to fromIntegral
                        * (read . (: []) . last . show
                               $ theta
                               / (-tau / cfg ^. sectSegQnt . to fromIntegral)
                          )
                        )
                        (-tau
                         / 4
                         - atan2
                               (abs dotProd)
                               ( cfg
                                     ^. prefEdBodyEditingSqrSideLen
                                     . to fromIntegral
                               / 2
                               )
                        )
                        theta
        sectMat <- gzmMat $ toJSVal sectClr
        sect <- Three.mesh sectGeom sectMat
        sectPos <- Three.pos sect
        Three.setY sectPos 0.001
        slctedEphemPrefWGzmRot <- Three.rot sect
        Three.setX slctedEphemPrefWGzmRot $ -tau / 4
        Three.add sectPrnt sect
        return sect
      where
        theta
            = atan2
                  (abs dotProd)
                  (cfg ^. prefEdBodyEditingSqrSideLen . to fromIntegral / 2)
            * 2
    drawTrigGzm
        circTrigGzmClr
        circTrigGzmPrnt
        (CircleTrigger (V.Vector2 x y) (Radius r))
        = do
        circTrigGzmGeom <- Three.circGeom
                               r
                               (cfg ^. sectSegQnt . to fromIntegral * round r)
                               0
                               tau
        circTrigGzmMat <- gzmMat circTrigGzmClr
        circTrigGzm <- Three.mesh circTrigGzmGeom circTrigGzmMat
        circTrigGzmPos <- Three.pos circTrigGzm
        Three.setX circTrigGzmPos x
        Three.setY circTrigGzmPos 0.001
        Three.setZ circTrigGzmPos y
        circTrigGzmRot <- Three.rot circTrigGzm
        Three.setX circTrigGzmRot $ -tau / 4
        Three.add circTrigGzmPrnt circTrigGzm
    drawTrigGzm
        dotProdTrigGzmClr
        dotProdTrigGzmPrnt
        (UnitSectorTrigger _ dotProd1)
        = do
        dotProdTrigGzm <- drawSect dotProdTrigGzmClr dotProdTrigGzmPrnt
                              $ unUnitDotProduct dotProd1
                              * ( cfg
                                      ^. prefEdBodyEditingSqrSideLen
                                      . to fromIntegral
                                / 2
                                )
        dotProdTrigGzmPos <- Three.pos dotProdTrigGzm
        Three.setZ dotProdTrigGzmPos
            $ cfg ^. prefEdBodyEditingSqrSideLen . to fromIntegral / (-2)
    drawTrigGzm cellTrigGzmClr cellTrigGzmPrnt cellTrig
        = do
        cellTrigGzmGeom <- Three.plnGeom 1 1
        cellTrigGzmMat <- gzmMat cellTrigGzmClr
        cellTrigGzm <- Three.mesh cellTrigGzmGeom cellTrigGzmMat
        cellTrigGzmPos <- Three.pos cellTrigGzm
        Three.setX cellTrigGzmPos 0.5 >> Three.setZ cellTrigGzmPos 0.5
        cellTrigGzmOrg <- Three.grp
        case cellTrig of
            CellRectangleTrigger (V.Vector2 sRow sClm) (V.Vector2 eRow eClm)
                ->
                adjTrigGzmOrg cellTrigGzmOrg sClm sRow eClm eRow
            CellSquareTrigger sideLen (V.Vector2 sRow sClm)
                ->
                adjTrigGzmOrg cellTrigGzmOrg sClm sRow sideLen sideLen
            CellTrigger (V.Vector2 sRow sClm)
                ->
                adjTrigGzmOrg cellTrigGzmOrg sClm sRow (1 :: Int) (1 :: Int)
            _
                ->
                return ()
        Three.add cellTrigGzmOrg cellTrigGzm
        Three.add cellTrigGzmPrnt cellTrigGzmOrg
    drawTrigGzms playerIsInsTrigAct pose pref
        = do
        case Reaction <$> playerIsInsTrigAct <*> pose
            of
            Just react@(Reaction playerInsTrigAct _)
                -> do
                mCtx <- liftIO . readIORef $ mCtxIoRef
                trigGzmGrp <- Three.getObjByName (mCtx ^. scn) "trigGzms"
                Three.clr trigGzmGrp
                mapM_
                    ( drawTrigGzm
                         (cfg ^. trigGzmClrs . at playerInsTrigAct . non "")
                         trigGzmGrp
                    . _singleTrigger
                    )
                    $ pref
                          ^. _LifefulPrefab
                          .  identity
                          .  body
                          .  at react
                          .  non []
                    ++ pref ^. _StaticPrefab . body . at react . non []
            Nothing
                -> do
                return ()
    exclSlctedPref prefName prefFileCont
        =
        (unlines (init $ lines prefFileCont) ++)
        . (++ "]")
        . ("all=[" ++)
        . List.intercalate ","
        . filter (/= prefName)
        . splitOn ","
        . init
        . tail
        . dropWhile ('[' /=)
        . last
        . lines
        $ prefFileCont
    gzmMat trigGrpClr
        =
        Three.meshBscMat
        =<< toJSVal
                (JsApi.Prms
                     [ ("color", toJSVal trigGrpClr)
                     , ("depthWrite", toJSVal False)
                     , ("opacity", toJSVal (0.5 :: Float))
                     , ("transparent", toJSVal True)
                     ]
                )
    tau = pi * 2
    viewPrefName pref
        =
        (pref
         ^? _LifefulPrefab . identity . name
         ^. to (<|> pref ^? _StaticPrefab . name)
        )
