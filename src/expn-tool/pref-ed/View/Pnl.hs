{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module View.Pnl where


import Act
import Bem.Init
import Core.Object
import Core.Trigger
import View.PnlHeader
import View.PnlSect
import qualified Core.Prefab as Pref
import qualified Lens
import qualified Pref

import Bem.Miso.View.Html
import Control.Lens
import Control.Monad.Reader
import Data.Map
import Data.Maybe
import Miso
import Miso.String


mkPnl :: MkNoModsBlk
mkPnl = do
    mbSlctedInsTrigAct <- asks (^. Lens.slctedPlayerIsInsTrigAct)
    mbSlctedPose <- asks (^. Lens.slctedPose)
    slctedPref' <- asks (^. Lens.slctedPref)
    slctedPrefBodyEdAct' <- asks (^. Lens.slctedPrefBodyEdAct)
    slctedPrefCopy' <- asks (^. Lens.slctedPrefCopy)
    _mkNoModsBlk viewGens (NonVoidHtmlElem div_)
        ( []
        , [ pnlHeader
                "Prefabs"
                slctedPref'
                slctedPrefCopy'
                (elems Pref.prefs)
                [ option_ [disabled_ True] [text "Prefab type"]
                , _noElemModsMix viewGens (NonVoidHtmlElem option_)
                      ( [ value_ . ms . show
                              $ (Pref.StaticPrefab
                                     (Pref.PrefabIdentity mempty mempty)
                                :: Pref.Prefab String Double Int
                                )
                        ]
                      , [text "Static"]
                      )
                      Btn
                      (case slctedPref' of
                           Pref.StaticPrefab _ -> [Slcted]
                           _ -> [Dang]
                      )
                      Pnl
                      Pnl_Btn
                , _noElemModsMix viewGens (NonVoidHtmlElem option_)
                      ( [ value_ . ms . show
                              $ (Pref.LifefulPrefab
                                    (Pref.PrefabWithProperties
                                         (Pref.PrefabIdentity mempty mempty)
                                         (Pref.LifefulPrefabProperties mempty)
                                    )
                                :: Pref.Prefab String Double Int
                                )
                        ]
                      , [text "Lifeful"]
                      )
                      Btn
                      (case slctedPref' of
                           Pref.LifefulPrefab _ -> [Slcted]
                           _ -> []
                      )
                      Pnl
                      Pnl_Btn
                ]
                Pnl
                Pnl_Header
            , _noModsElem viewGens (NonVoidHtmlElem section_)
                ( []
                , [ pnlSect
                        "Trigger"
                        [ _noBlkModsMix viewGens (NonVoidHtmlElem select_)
                              ( [onInput $ OnlySimpAct . UnsafeSlctXAct]
                              ,
                                  [ option_
                                        [ disabled_ True
                                        , selected_
                                              $ isNothing mbSlctedInsTrigAct
                                        ]
                                        [text "Inside action"]
                                  , _noElemModsMix
                                        viewGens
                                        (NonVoidHtmlElem option_)
                                        ( [value_ . ms $ show KeepEnemiesAway]
                                        , [text "Keep enemies away"]
                                        )
                                        Btn
                                        (if mbSlctedInsTrigAct
                                         == pure KeepEnemiesAway
                                         then [Slcted]
                                         else []
                                        )
                                        Pnl
                                        Pnl_Btn
                                  , _noElemModsMix
                                        viewGens
                                        (NonVoidHtmlElem option_)
                                        ( [value_ . ms $ show KeepEveryoneAway]
                                        , [text "Keep everyone away"]
                                        )
                                        Btn
                                        (if mbSlctedInsTrigAct
                                         == pure KeepEveryoneAway
                                         then [Slcted]
                                         else []
                                        )
                                        Pnl
                                        Pnl_Btn
                                  ]
                              )
                              Btn
                              Pnl
                              Pnl_Btn
                              [PnlBtn_Size Big, PnlBtn_Vert]
                        , _noBlkModsMix viewGens (NonVoidHtmlElem select_)
                              ( [onInput $ OnlySimpAct . UnsafeSlctHumanPose]
                              , [ option_
                                      [ disabled_ True
                                      , selected_ $ isNothing mbSlctedPose
                                      ]
                                      [text "Pose"]
                                , option_
                                      [value_ . ms $ show ForwardsWedgedArms]
                                      [text "Forwards wedged arms"]
                                , option_
                                      [value_ . ms $ show ParalleledArms]
                                      [text "Paralleled arms"]
                                ]
                              )
                              Btn
                              Pnl
                              Pnl_Btn
                              [PnlBtn_Size Big, PnlBtn_Vert]
                        , _noBlkModsMix viewGens (NonVoidHtmlElem select_)
                              ( [onInput $ OnlySimpAct . UnsafeSlctSingTrig]
                              ,
                                  [ option_
                                        [ disabled_ True
                                        , selected_
                                              $ slctedPrefBodyEdAct' == View
                                        ]
                                        [text "Shape"]
                                  , option_
                                        [ value_ . ms . show
                                              $ (CellRectangleTrigger
                                                     mempty
                                                     mempty
                                                 :: SingleTrigger Double Int
                                                )
                                        ]
                                        [text "Cell"]
                                  , option_
                                        [ value_ . ms . show
                                              $ (CircleTrigger
                                                     mempty
                                                     mempty
                                                 :: SingleTrigger Double Int
                                                )
                                        ]
                                        [text "Circle"]
                                  , option_
                                        [ value_ . ms . show
                                              $ (HorizontalUnitSectorTrigger
                                                     mempty
                                                 :: SingleTrigger Double Int
                                                )
                                        ]
                                        [text "Horizontal sector"]
                                  , option_
                                        [ value_ . ms . show
                                              $ (UnitSectorTrigger
                                                     mempty
                                                     mempty
                                                 :: SingleTrigger Double Int
                                                )
                                        ]
                                        [text "Sector"]
                                  ]
                              )
                              Btn
                              Pnl
                              Pnl_Btn
                              [PnlBtn_Size Big, PnlBtn_Vert]
                           , _noBlkModsMix viewGens (NonVoidHtmlElem select_)
                                 ( [ onInput
                                         $ OnlySimpAct
                                         . UnsafeSlctPrefBodyEdAct
                                   ]
                                 , [ option_
                                         [disabled_ True, selected_ True]
                                         [text "Grouping criterion"]
                                   , _noModsMix viewGens (NonVoidHtmlElem option_)
                                         ( [ value_ . ms . show
                                                 $ AddTrigGrp
                                                       (SameHorizontalSizeGroupCommon
                                                            mempty
                                                       )
                                           ]
                                         , [text "Horizontal size"]
                                         )
                                         Btn
                                         Pnl
                                         Pnl_Btn
                                   , _noModsMix viewGens (NonVoidHtmlElem option_)
                                         ( [ value_ . ms . show
                                                 $ AddTrigGrp
                                                       (SameVerticalSizeGroupCommon
                                                            mempty
                                                       )
                                           ]
                                         , [text "Vertical size"]
                                         )
                                         Btn
                                         Pnl
                                         Pnl_Btn
                                   ]
                                 )
                                 Btn
                                 Pnl
                                 Pnl_Btn
                                 [PnlBtn_Size Big, PnlBtn_Vert]
                           , _noBlkModsMix viewGens (NonVoidHtmlElem button_)
                                 ( [onClick $ OnlySimpAct RlsTrigGrp]
                                 , [text "Release group"]
                                 )
                                 Btn
                                 Pnl
                                 Pnl_Btn
                                 [PnlBtn_Size Big, PnlBtn_Vert]
                        ]
                        Pnl
                        Pnl_Sect
                        $ if slctedPrefBodyEdAct' == View
                          || isNothing mbSlctedInsTrigAct
                          || isNothing mbSlctedPose
                          then [PnlSect_Blked]
                          else []
                     , pnlSect
                           "Properties"
                            (case
                                 slctedPref'
                                     ^? Lens._LifefulPrefab
                                     . Lens.properties
                                     . Lens.runSpeed
                                 of
                                 Just runSpd ->
                                     [ _noModsMix viewGens (VoidHtmlElem input_)
                                           [ onInput
                                                 $ OnlySimpAct
                                                 . AltSlctedLiflPrefRunSpd
                                           , value_ . ms . show
                                                 $ Pref.unRunSpeed runSpd
                                           ]
                                           Btn
                                           Pnl
                                           Pnl_Btn
                                     ]
                                 Nothing -> []
                           )
                           Pnl
                           Pnl_Sect
                           []
                  ]
                )
                Pnl
                Pnl_Body
          ]
        )
