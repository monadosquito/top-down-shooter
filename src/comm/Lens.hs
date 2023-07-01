{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}


module Lens where


import Cfg
import Core.Prefab
import Core.Trigger
import Mdl

import Control.Lens


makeFieldsNoPrefix ''Cfg
makeFieldsNoPrefix ''LifefulPrefabProperties
makeFieldsNoPrefix ''MCtx
makeFieldsNoPrefix ''Mdl
makeFieldsNoPrefix ''PrefabIdentity
makeFieldsNoPrefix ''PrefabWithProperties
makeFieldsNoPrefix ''SingleTrigger
makePrisms ''AnyTriggerLike
makePrisms ''SingleTrigger
makePrisms ''TriggerLike
makePrisms ''Prefab
