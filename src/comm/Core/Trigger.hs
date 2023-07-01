{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}


module Core.Trigger
    ( AnyTriggerLike (..)
    , CriterionGroupedTrigger (..)
    , PlayerIsInsideTriggerAction (..)
    , Radius (..)
    , SingleTrigger (..)
    , SingleTriggerGroupability (..)
    , TriggerGroupCommon (..)
    , TriggerGroupingCriterion (..)
    , TriggerLike (..)
    , UnitDotProduct
    , unUnitDotProduct
    , unitDotProduct
    , initialize
    , trim
    , tryToAdd
    ) where


import Core.Configuration
import Core.Coordinate
import Core.TriggerInternal
import Core.Vector

import Control.Applicative
import Data.Kind
import Text.Read


newtype Radius a = Radius a
deriving instance (Eq a, EnabledFlag 'NeedComparison) => Eq (Radius a)
deriving instance (Show a, EnabledFlag 'NeedSerialization) => Show (Radius a)
deriving instance (Read a, EnabledFlag 'NeedSerialization) => Read (Radius a)
instance Num n => Semigroup (Radius n) where
    Radius radiusA <> Radius radiusB = Radius (radiusA + radiusB)
instance Num n => Monoid (Radius n) where
    mempty = Radius 0 

data AnyTriggerLike a b where
         AnySingleTrigger
             :: { _singleTrigger :: SingleTrigger a b
                }
             -> AnyTriggerLike a b
         AnyTriggerCriterionGroup
             :: TriggerLike
                    a
                    b
                    ('TriggerCriterionGroup triggerGroupingCriterion)
             -> AnyTriggerLike a b
deriving instance (Show a, Show b) => Show (AnyTriggerLike a b)
instance (Eq a, Eq b) => Eq (AnyTriggerLike a b) where
    AnySingleTrigger singleTriggerA == AnySingleTrigger singleTriggerB
        =
        singleTriggerA == singleTriggerB
    AnyTriggerCriterionGroup
        (SameHorizontalSizeTriggerGroup commonA singleTriggersA)
        ==
        AnyTriggerCriterionGroup
            (SameHorizontalSizeTriggerGroup commonB singleTriggersB)
        =
        commonA == commonB && singleTriggersA == singleTriggersB
    _ == _ = False
instance (Read a, Read b) => Read (AnyTriggerLike a b) where
    readPrec
        =
        parens
            $ lexP
            >>= \case
                    Ident "AnySingleTrigger"
                        ->
                        AnySingleTrigger <$> readPrec
                    Ident "AnyTriggerCriterionGroup"
                        ->
                        AnyTriggerCriterionGroup
                        <$> (readPrec
                             :: Read a
                             => ReadPrec
                                    (TriggerLike
                                         a
                                         b
                                         ('TriggerCriterionGroup
                                              'SameHorizontalSize
                                         )
                                    )
                            )
                        <|> AnyTriggerCriterionGroup
                        <$> (readPrec
                             :: Read a
                             => ReadPrec
                                    (TriggerLike
                                         a
                                         b
                                         ('TriggerCriterionGroup
                                              'SameVerticalSize
                                         )
                                    )
                            )
                    _
                        ->
                        empty

data CriterionGroupedTrigger :: Type -> Type -> TriggerGroupingCriterion -> Type
  where
    SameHorizontalSizeUnitSectorTrigger
        :: UnitDotProduct a
        -> CriterionGroupedTrigger a b 'SameHorizontalSize
    SameVerticalSizeUnitSectorTrigger
        :: UnitDotProduct a
        -> CriterionGroupedTrigger a b 'SameVerticalSize
deriving instance
    Eq a
    =>
    Eq (CriterionGroupedTrigger a b triggerGroupingCriterion)
deriving instance (Show a, Show b, EnabledFlag 'NeedSerialization)
    =>
    Show (CriterionGroupedTrigger a b triggerGroupingCriterion)
instance Read a
    =>
    Read (CriterionGroupedTrigger a b 'SameHorizontalSize)
  where
    readPrec
        =
        parens
            $   lexP
            >>= \case
                    Ident "SameHorizontalSizeUnitSectorTrigger"
                        ->
                        SameHorizontalSizeUnitSectorTrigger <$> readPrec
                    _
                        ->
                        empty
instance Read a => Read (CriterionGroupedTrigger a b 'SameVerticalSize)
  where
    readPrec
        =
        parens
            $   lexP
            >>= \case
                    Ident "SameVerticalSizeUnitSectorTrigger"
                        ->
                        SameVerticalSizeUnitSectorTrigger <$> readPrec
                    _
                        ->
                        empty

data PlayerIsInsideTriggerAction = KeepEnemiesAway | KeepEveryoneAway
deriving instance EnabledFlag 'NeedComparison => Eq PlayerIsInsideTriggerAction
deriving instance EnabledFlag 'NeedComparison => Ord PlayerIsInsideTriggerAction
deriving instance
    EnabledFlag 'NeedSerialization
    =>
    Read PlayerIsInsideTriggerAction
deriving instance
    EnabledFlag 'NeedSerialization
    =>
    Show PlayerIsInsideTriggerAction

data SingleTrigger a b
         = CellRectangleTrigger (Cell b) (Cell b)
         | CellSquareTrigger b (Cell b)
         | CellTrigger (Cell b)
         | CircleTrigger (Position a) (Radius a)
         | HorizontalUnitSectorTrigger (UnitDotProduct a)
         | UnitSectorTrigger (UnitDotProduct a) (UnitDotProduct a)
         | VerticalUnitSectorTrigger (UnitDotProduct a)
deriving instance
    (Eq a, Eq b, EnabledFlag 'NeedComparison)
    =>
    Eq (SingleTrigger a b)
deriving instance
    (Read a, Read b, EnabledFlag 'NeedSerialization)
    =>
    Read (SingleTrigger a b)
deriving instance
    (Show a, Show b, EnabledFlag 'NeedSerialization)
    =>
    Show (SingleTrigger a b)

data SingleTriggerGroupability a
         = NonGroupableSingleTrigger
         | TriggerCriterionGroup a

data TriggerGroupCommon :: Type -> TriggerGroupingCriterion -> Type where
    SameHorizontalSizeGroupCommon
        :: UnitDotProduct a
        -> TriggerGroupCommon a 'SameHorizontalSize
    SameVerticalSizeGroupCommon
        :: UnitDotProduct a
        -> TriggerGroupCommon a 'SameVerticalSize
deriving instance
    (Eq a, EnabledFlag 'NeedComparison)
    =>
    Eq (TriggerGroupCommon a triggerGroupingCriterion)
deriving instance
    (Show a, EnabledFlag 'NeedSerialization)
    =>
    Show (TriggerGroupCommon a triggerGrpoupingCriterion)
instance Read a => Read (TriggerGroupCommon a 'SameHorizontalSize) where
    readPrec
        =
        parens
            $   lexP
            >>= \case
                    Ident "SameHorizontalSizeGroupCommon"
                        ->
                        SameHorizontalSizeGroupCommon <$> readPrec
                    _
                        ->
                        empty
instance Read a => Read (TriggerGroupCommon a 'SameVerticalSize) where
    readPrec
        =
        parens
            $   lexP
            >>= \case
                    Ident "SameVerticalSizeGroupCommon"
                        ->
                        SameVerticalSizeGroupCommon <$> readPrec
                    _
                        ->
                        empty

data TriggerGroupingCriterion
         = SameHorizontalSize
         | SameVerticalSize
deriving instance EnabledFlag 'NeedComparison => Eq TriggerGroupingCriterion
deriving instance
    EnabledFlag 'NeedSerialization
    =>
    Read TriggerGroupingCriterion
deriving instance
    EnabledFlag 'NeedSerialization
    =>
    Show TriggerGroupingCriterion

data TriggerLike
         :: Type
         -> Type
         -> SingleTriggerGroupability TriggerGroupingCriterion
         -> Type
  where
        SameHorizontalSizeTriggerGroup
            :: TriggerGroupCommon a 'SameHorizontalSize
            -> [CriterionGroupedTrigger a b 'SameHorizontalSize]
            -> TriggerLike a b ('TriggerCriterionGroup 'SameHorizontalSize)
        SameVerticalSizeTriggerGroup
            :: TriggerGroupCommon a 'SameVerticalSize
            -> [CriterionGroupedTrigger a b 'SameVerticalSize]
            -> TriggerLike a b ('TriggerCriterionGroup 'SameVerticalSize)
        SingleTrigger
            :: SingleTrigger a b
            -> TriggerLike a b 'NonGroupableSingleTrigger
deriving instance
    (Eq a, Eq b, EnabledFlag 'NeedComparison)
    =>
    Eq (TriggerLike a b singleTriggerGroupability)
deriving instance
    (Show a, Show b, EnabledFlag 'NeedSerialization)
    =>
    Show (TriggerLike a b singleTriggerGroupability)
instance
    Read a
    =>
    Read (TriggerLike a b ('TriggerCriterionGroup 'SameHorizontalSize))
  where
    readPrec = parens
             $ lexP
             >>= \case
                     Ident "SameHorizontalSizeTriggerGroup"
                         ->
                         SameHorizontalSizeTriggerGroup
                         <$> readPrec
                         <*> readPrec
                     _
                         ->
                         empty
instance
    Read a
    =>
    Read (TriggerLike a b ('TriggerCriterionGroup 'SameVerticalSize))
  where
    readPrec
        = parens
            $ lexP
            >>= \case
                    Ident "SameVerticalSizeTriggerGroup"
                        ->
                        SameVerticalSizeTriggerGroup <$> readPrec <*> readPrec
                    _
                        ->
                        empty

-- group :: [SingleTrigger a b] -> [AnyTriggerLike a b]
-- group singleTriggers = filter () singleTriggers
--   where
--     pred (UnitSectorTrigger s _) = 
--     pred _ = False
    


initialize
    :: TriggerGroupCommon a triggerGroupingCriterion
    -> TriggerLike a b ('TriggerCriterionGroup triggerGroupingCriterion)
initialize (SameHorizontalSizeGroupCommon common)
    =
    SameHorizontalSizeTriggerGroup (SameHorizontalSizeGroupCommon common) mempty
initialize (SameVerticalSizeGroupCommon common)
    =
    SameVerticalSizeTriggerGroup (SameVerticalSizeGroupCommon common) mempty

trim :: Eq b => SingleTrigger a b -> SingleTrigger a b
trim (CellRectangleTrigger startCell endCell@(Vector2 endRow endColumn))
    | startCell == endCell = CellTrigger startCell
    | endRow == endColumn = CellSquareTrigger endRow startCell
trim trigger = trigger

tryToAdd
    :: SingleTrigger a b
    -> TriggerLike a b ('TriggerCriterionGroup triggerGroupingCriterion)
    -> SingleTriggerGroupability
           (TriggerLike a b ('TriggerCriterionGroup triggerGroupingCriterion))
tryToAdd
    (UnitSectorTrigger _ horizontalUnitDotProduct)
    (SameHorizontalSizeTriggerGroup common triggers)
    =
    TriggerCriterionGroup
        (SameHorizontalSizeTriggerGroup
             common
             (triggers
              ++ [SameHorizontalSizeUnitSectorTrigger horizontalUnitDotProduct]
             )
        )
tryToAdd
    (UnitSectorTrigger verticalUnitDotProduct _)
    (SameVerticalSizeTriggerGroup common triggers)
    =
    TriggerCriterionGroup
        (SameVerticalSizeTriggerGroup
             common
             (triggers
              ++ [SameVerticalSizeUnitSectorTrigger verticalUnitDotProduct]
             )
        )
tryToAdd (CellRectangleTrigger _ _) _ = NonGroupableSingleTrigger
tryToAdd (CellSquareTrigger _ _) _ = NonGroupableSingleTrigger
tryToAdd (CellTrigger _) _ = NonGroupableSingleTrigger
tryToAdd (CircleTrigger _ _) _ = NonGroupableSingleTrigger
tryToAdd (HorizontalUnitSectorTrigger _) _ = NonGroupableSingleTrigger
tryToAdd (VerticalUnitSectorTrigger _) _ = NonGroupableSingleTrigger

unUnitDotProduct :: UnitDotProduct a -> a
unUnitDotProduct (UnitDotProduct unitDotProduct') = unitDotProduct'

unitDotProduct :: (Num n, Ord n) => n -> UnitDotProduct n
unitDotProduct p
    | p <= 1 && p >= -1 = UnitDotProduct p
    | otherwise = error "non-unit dot product"
