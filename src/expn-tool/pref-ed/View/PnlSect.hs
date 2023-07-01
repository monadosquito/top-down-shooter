{-# LANGUAGE RankNTypes #-}


module View.PnlSect where


import Act
import Bem.Init

import Bem.Miso.View.Html
import Miso
import Miso.String


pnlSect :: Lbl -> [ListItem trigGrpingCritType] -> Elem
pnlSect lbl listItems
    =
    _elem viewGens (NonVoidHtmlElem section_)
        ( []
        , [ _noModsElem viewGens (NonVoidHtmlElem label_)
                ([], [text lbl])
                Pnl
                Pnl_SectLbl
          , _noModsElem viewGens (NonVoidHtmlElem ul_)
                ([], listItems)
                Pnl
                Pnl_SectItemList
          ]
        )


type Lbl = MisoString
type ListItem trigGrpingCritType = View Act'
