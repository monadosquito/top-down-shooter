{-# LANGUAGE PackageImports #-}


module WwwEnv where


import "ghcjs-dom" GHCJS.DOM.Document
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLLinkElement
import GHCJS.DOM.HTMLScriptElement
import GHCJS.DOM.Node
import GHCJS.DOM.Types
import Language.Javascript.JSaddle.Warp
import Network.Wai
import Network.Wai.Application.Static
import Network.WebSockets.Connection
import System.Environment


inclPub :: AppPartName -> JSM () -> JSM ()
inclPub appPartName app = do
    mbDoc <- currentDocument
    case mbDoc of
        Just doc -> do
            mbHead <- getHead doc
            case mbHead of
                Just head' -> do
                    link <- uncheckedCastTo HTMLLinkElement
                            <$> createElement doc "link"
                    setHref link
                        $ "auto/dev-tmp/" ++ appPartName ++ "-style.css"
                    setRel link "stylesheet"
                    appendChild_ head' link

                    jsMainScriptFiles <- words
                                         <$> liftIO
                                                 (getEnv
                                                      "usedJsDepScriptLclPaths"
                                                 )
                    forM_ (map ("auto/dev-tmp/" ++) jsMainScriptFiles)
                        $ \scriptSrc -> do
                              liftIO $ threadDelay 100000
                              libScript <- uncheckedCastTo HTMLScriptElement
                                  <$> createElement doc "script"
                              setSrc libScript scriptSrc
                              appendChild_ head' libScript
                    app
                Nothing -> return ()
        Nothing -> return ()

loadPub :: AppPartName -> JSM () -> IO Application
loadPub appPartName app
    = jsaddleWithAppOr defaultConnectionOptions (inclPub appPartName app)
    . staticApp
    $ defaultWebAppSettings "data/pub"


type AppPartName = String
