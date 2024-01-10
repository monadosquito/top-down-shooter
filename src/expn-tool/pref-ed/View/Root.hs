{-# LANGUAGE OverloadedStrings #-}


module View.Root where


import Act
import Mdl
import View.Pnl

import Control.Monad.Reader
import Miso

import Bem.Init

import Bem.Miso.Utl.Utl


root :: Reader Mdl (View Act')
root = do
    NoModsBlkElem pnl <- mkPnl
    return $ div_
                 [class_ "Root"]
                 [canvas_ [id_ "canv"] [], pnl Pnl Root Root_Pnl []]
