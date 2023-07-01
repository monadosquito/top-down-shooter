module JsApi.Comm.Ffi where


import GHCJS.Nullable
import Language.Javascript.JSaddle
import Numeric.Natural


head :: JSVal -> Maybe JSVal
head = nullableToMaybe . head'

last :: JSVal -> Maybe JSVal
last = nullableToMaybe . last'


foreign import javascript
    "$1[0]"
    head' :: JSVal -> Nullable JSVal

foreign import javascript
    "$1.indexOf($2)"
    ixOf :: JSVal -> JSVal -> IO Int

foreign import javascript
    "$1[$1.length - 1]"
    last' :: JSVal -> Nullable JSVal

foreign import javascript
    "globalThis.window"
    win :: IO JSVal
