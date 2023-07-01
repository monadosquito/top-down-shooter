{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}


module Cfg where


import Core.Trigger

import Data.Map
import Numeric.Natural


data Cfg = Cfg
               { _canvH' :: Int -> Double
               , _prefEdBodyEditingSqrSideLen :: Natural
               , _sectSegQnt :: Natural
               , _slctedEphemPrefWGzmClr :: String
               , _trigGzmClrs :: Map PlayerIsInsideTriggerAction String
               , pnlHPercent :: Natural
               }


cfg :: Cfg
cfg = Cfg
          { _canvH' = (* (100 - fromIntegral (pnlHPercent cfg)))
                    . (/ 100)
                    . fromIntegral
          , _prefEdBodyEditingSqrSideLen = 99
          , _sectSegQnt = 8
          , _slctedEphemPrefWGzmClr = "firebrick"
          , _trigGzmClrs = fromList
                               [ (KeepEnemiesAway , "firebrick")
                               , (KeepEveryoneAway , "gray")
                               ]
          , pnlHPercent = 20
          }
