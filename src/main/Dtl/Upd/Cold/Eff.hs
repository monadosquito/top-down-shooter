{-# Language PackageImports #-}


module Dtl.Upd.Cold.Eff where


import Dtl.Act.Cold.Eff
import Dtl.Mdl.Cold

import GHCJS.DOM
import GHCJS.DOM.Element
import qualified "ghcjs-dom" GHCJS.DOM.Document as Doc
import qualified Miso


upd :: Act -> Mdl -> Miso.JSM ()
upd ColdEff _ = do
    Just doc <- currentDocument
    Just body <- Doc.getBody doc
    setInnerHTML body "Cold application"
