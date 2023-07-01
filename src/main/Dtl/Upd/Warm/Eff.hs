{-# Language PackageImports #-}


module Dtl.Upd.Warm.Eff where


import Dtl.Act.Warm.Eff
import Dtl.Mdl.Warm

import "ghcjs-dom" GHCJS.DOM.Document
import GHCJS.DOM
import GHCJS.DOM.Element
import Miso hiding (getBody)


upd :: Act -> Mdl -> JSM ()
upd WarmEff _ = do
    Just doc <- currentDocument
    Just body <- getBody doc
    setInnerHTML body "Warm!"
    return ()
