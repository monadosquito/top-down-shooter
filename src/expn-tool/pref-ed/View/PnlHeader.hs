{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module View.PnlHeader where


import Act
import Bem.Init
import Core.Prefab
import Lens

import Bem.Miso.View.Html
import Control.Applicative
import Control.Lens
import Miso
import qualified Miso.String as Ms


pnlHeader
    :: PrefMenuLbl
    -> SlctedPref
    -> SlctedPrefBackup
    -> [Prefab String Double Int]
    -> [PrefTypeOpt trigGrpingCritType]
    -> NoModsElem
pnlHeader prefMenuLbl slctedPref' slctedPrefBackup' prefs prefTypeOpts
    =
    _noModsElem viewGens (NonVoidHtmlElem header_)
        ( []
        , [ _noModsMix viewGens (NonVoidHtmlElem select_)
                ( [onInput $ OnlySimpAct . SlctPref]
                , case traverse (fmap Ms.ms . viewPrefName) prefs of
                      Just prefNames ->
                          prefMenuLblOpt
                          : map
                                (\prefName' ->
                                     _noElemModsMix
                                         viewGens
                                         (NonVoidHtmlElem option_)
                                         ([], [text prefName'])
                                         Btn
                                         [ if pure prefName'
                                           == fmap
                                                  Ms.ms
                                                  (viewPrefName slctedPref')
                                           then Slcted
                                           else Dang
                                         ]
                                         Pnl
                                         Pnl_Btn
                                )
                                prefNames
                      Nothing -> [prefMenuLblOpt]
                )
                Btn
                Pnl
                Pnl_Btn
          ]
          ++ case viewPrefName slctedPref' of
                 Just slctedPrefName ->
                     [ _noModsMix viewGens (VoidHtmlElem input_)
                           [ id_ "slctedPrefName"
                           , onInput
                                 $ OnlySimpAct
                                 . ChngPrefName
                                 . Ms.fromMisoString
                           , placeholder_ "Selected prefab name"
                           , value_ . Ms.ms $ slctedPrefName
                           ]
                           Btn
                           Pnl
                           Pnl_TextInput
                     ]
                 Nothing -> []
          ++ if null prefTypeOpts
             then []
             else [ _noModsMix viewGens (NonVoidHtmlElem select_)
                        ([onInput $ OnlySimpAct . SlctPrefType], prefTypeOpts)
                        Btn
                        Pnl
                        Pnl_Btn
                  ]
          ++ [ _noModsElem viewGens (NonVoidHtmlElem section_)
                   ( []
                   , (if slctedPref' == slctedPrefBackup'
                      then
                          _noModsMix viewGens (NonVoidHtmlElem button_)
                              ([], [text "Save"])
                              Btn
                              Pnl
                              Pnl_Btn
                      else
                          _noElemModsMix viewGens (NonVoidHtmlElem button_)
                              ([onClick $ OnlySimpAct SavePref], [text "Save"])
                              Btn
                              [Dang]
                              Pnl
                              Pnl_Btn
                     )
                     : case viewPrefName slctedPref' of
                            Just slctedPrefName ->
                                [ _noElemModsMix
                                      viewGens
                                      (NonVoidHtmlElem button_)
                                      ( [ onClick
                                              $ OnlySimpAct
                                                    (DelPref
                                                         (Ms.ms slctedPrefName)
                                                    )
                                        ]
                                      , [text "Delete"]
                                      )
                                      Btn
                                      [Dang]
                                      Pnl
                                      Pnl_Btn
                                ]
                            Nothing -> []
                   )
                   Pnl
                   Pnl_ActSect
               ]
        )
  where
    prefMenuLblOpt
        =
        _noModsElem viewGens (NonVoidHtmlElem option_)
            ([disabled_ True, selected_ True], [text prefMenuLbl])
            Pnl
            Pnl_PrefMenuLbl
    viewPrefName pref
        =
        pref
        ^? _LifefulPrefab . identity . name
        ^. to (<|> pref ^? _StaticPrefab . name)


type PrefMenuLbl = Ms.MisoString
type PrefTypeOpt trigGrpingCritType = View Act'
type SlctedPref = Prefab String Double Int
type SlctedPrefBackup = SlctedPref
