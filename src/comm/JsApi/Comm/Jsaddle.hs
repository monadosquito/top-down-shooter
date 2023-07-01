module JsApi.Comm.Jsaddle where


import Control.Monad
import qualified Language.Javascript.JSaddle as Jsaddle


head :: Jsaddle.JSVal -> Jsaddle.JSM (Maybe Jsaddle.JSVal)
head arr = do
    head' <- arr Jsaddle.!! 0
    (head' <$) . guard . not <$> Jsaddle.valIsUndefined head'

ixOf :: Jsaddle.JSVal -> Jsaddle.JSVal -> Jsaddle.JSM Int
ixOf arr val
    =
    round
    <$> (Jsaddle.valToNumber =<< (arr Jsaddle.# "indexOf" $ Jsaddle.toJSVal val)
        )

last :: Jsaddle.JSVal -> Jsaddle.JSM (Maybe Jsaddle.JSVal)
last arr = do
    length' <- round <$> (Jsaddle.valToNumber $ arr Jsaddle.! "length")
    last' <- arr Jsaddle.!! (length' - 1)
    (last' <$) . guard . not <$> Jsaddle.valIsUndefined last'

win :: Jsaddle.JSM Jsaddle.JSVal
win = Jsaddle.jsg "window"
