{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}


module Core.Prefab
    ( LifefulPrefabProperties (..)
    , LocationEnvironment (..)
    , Prefab (..)
    , PrefabIdentity (..)
    , PrefabWithProperties (..)
    , runSpeed
    , unRunSpeed
    , transform
    ) where


import Core.Configuration
import Core.Object
import Core.PrefabInternal
import Core.Reaction
import Core.Trigger

import Data.Map


type LifefulPrefab a b c = PrefabWithProperties
                               a
                               (Map Reaction [AnyTriggerLike b c])
                               (LifefulPrefabProperties b)

data LocationEnvironment a b c = LocationEnvironment
                                     { _dynamic :: [LifefulPrefab a b c]
                                     , _static :: [StaticObject a b]
                                     }
deriving instance
    (Eq a, Eq b, Eq c, EnabledFlag 'NeedComparison)
    =>
    Eq (LocationEnvironment a b c)
deriving instance
    (Read a, Read b, Read c, EnabledFlag 'NeedSerialization)
    =>
    Read (LocationEnvironment a b c)
deriving instance (Show a, Show b, Show c, EnabledFlag 'NeedSerialization)
    =>
    Show (LocationEnvironment a b c)

data Prefab a b c
         = LifefulPrefab (LifefulPrefab a b c)
         | LocationPrefab (PrefabIdentity a (LocationEnvironment a b c))
         | StaticPrefab (PrefabIdentity a (Map Reaction [AnyTriggerLike b c]))
deriving instance
    (Eq a, Eq b, Eq c, EnabledFlag 'NeedComparison)
    =>
    Eq (Prefab a b c)
deriving instance (Read a, Read b, Read c) => Read (Prefab a b c)
deriving instance (Show a, Show b, Show c, EnabledFlag 'NeedSerialization)
    =>
    Show (Prefab a b c)

data PrefabIdentity a b = PrefabIdentity {_body :: b, _name :: a} deriving Eq

deriving instance (Read a, Read b) => Read (PrefabIdentity a b)
deriving instance (Show a, Show b, EnabledFlag 'NeedSerialization)
    =>
    Show (PrefabIdentity a b)


runSpeed :: (Num s, Ord s) => s -> RunSpeed s
runSpeed s
    | s >= 0 = RunSpeed s
    | otherwise = error "negative run speed"

transform :: Num i => Prefab f i n -> Prefab f i n -> Prefab f i n
transform
    (LifefulPrefab (PrefabWithProperties identity _))
    (StaticPrefab (PrefabIdentity _ _))
    =
    StaticPrefab identity
transform (StaticPrefab identity) (LifefulPrefab _)
    =
    LifefulPrefab (PrefabWithProperties identity mempty)
transform fromPrefab _ = fromPrefab


data LifefulPrefabProperties a = LifefulPrefabProperties
                                     { _runSpeed :: RunSpeed a
                                     }
  deriving Eq

instance Num a => Semigroup (LifefulPrefabProperties a) where
    LifefulPrefabProperties runSpeedA <> LifefulPrefabProperties runSpeedB
        =
        LifefulPrefabProperties (runSpeedA <> runSpeedB)

instance Num a => Monoid (LifefulPrefabProperties a) where
    mempty = LifefulPrefabProperties mempty

deriving instance (Read a, EnabledFlag 'NeedSerialization)
    =>
    Read (LifefulPrefabProperties a)
deriving instance (Show a, EnabledFlag 'NeedSerialization)
    =>
    Show (LifefulPrefabProperties a)

data PrefabWithProperties a b c = PrefabWithProperties
                                      { _identity :: PrefabIdentity a b
                                      , _properties :: c
                                      }
  deriving Eq

deriving instance (Read a, Read b, Read c) => Read (PrefabWithProperties a b c)
deriving instance (Show a, Show b, Show c) => Show (PrefabWithProperties a b c)


unRunSpeed :: RunSpeed a -> a
unRunSpeed (RunSpeed runSpeed') = runSpeed'
