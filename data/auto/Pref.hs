{-# OPTIONS_GHC -w #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Pref where
import Core.Object
import Core.Prefab
import Core.Reaction
import Core.Trigger
import Core.TriggerInternal
import Core.Vector
import Data.Map
prefs=fromList [] :: Map String (Prefab String Double Int)
