{-# LANGUAGE RankNTypes #-}


module View.PnlSect where


import Act
import Bem.Init

import Bem.Miso.View.Html
import Miso
import Miso.String
import Bem.Miso.Utl.Utl


pnlSect :: Lbl -> [ListItem trigGrpingCritType] -> Elem
pnlSect lbl listItems
    =
    Bem.Init._elem viewGens (NonVoidHtmlElem section_)
        ( []
        , [ noModsElem (NonVoidHtmlElem label_)
                ([], [text lbl])
                Pnl
                Pnl_SectLbl
          , noModsElem (NonVoidHtmlElem ul_)
                ([], listItems)
                Pnl
                Pnl_SectItemList
          ]
        )


type Lbl = MisoString
type ListItem trigGrpingCritType = View Act'
