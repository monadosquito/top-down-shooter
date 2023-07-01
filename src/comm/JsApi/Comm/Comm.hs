{-# LANGUAGE CPP #-}


module JsApi.Comm.Comm where


import Control.Monad
#ifdef DEV
import Language.Javascript.JSaddle
#else
import GHCJS.Marshal
import GHCJS.Types
import JavaScript.Object.Internal
#endif
newtype Obj = Obj Object

instance ToJSVal Obj where
    toJSVal (Obj (Object obj')) = pure obj'

newtype Prms v = Prms [(JSString, v)]

instance (ToJSVal v) => ToJSVal (Prms v) where
    toJSVal (Prms prms) = do
        Obj obj' <- Obj <$> create
        forM_ prms $ \(k, v) -> do
            v' <- toJSVal v
            setProp k v' obj'
        toJSVal $ Obj obj'
