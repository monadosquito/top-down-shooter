{-# Language PackageImports #-}


module Dtl.Upd.Comm.Eff where


import Dtl.Act.Comm.Eff
import Dtl.Mdl.Comm

import "ghcjs-dom" GHCJS.DOM.Document
import GHCJS.DOM
import GHCJS.DOM.Element
import Miso hiding (getBody)


upd :: Act -> Mdl -> JSM ()
upd CommEff _ = do
    Just doc <- currentDocument
    Just body <- getBody doc
    setInnerHTML body "Hi!"
    return ()
