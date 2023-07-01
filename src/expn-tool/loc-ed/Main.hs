{-# LANGUAGE PackageImports #-}


import WwwEnv

import "ghcjs-dom" GHCJS.DOM.Document
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.Types


app :: JSM ()
app = do
    Just doc <- currentDocument
    Just body <- getBody doc
    setInnerHTML body "Location editor"
  where
    _ = inclPub
    _ = loadPub
