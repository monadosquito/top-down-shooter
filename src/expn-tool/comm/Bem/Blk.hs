-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}


module Bem.Blk where


import Bem.Utl.Utl


import Data.Kind


data Blk (a :: Type -> Type) b where
         Btn :: Blk NoElem BtnMod
         Pnl :: Blk PnlElem NoMod
         Root :: Blk RootElem NoMod
deriving instance Show (Blk a b)

data BtnMod = Dang | Slcted deriving Show

data PnlElem a where
         Pnl_ActSect :: PnlElem NoMod
         Pnl_Body :: PnlElem NoMod
         Pnl_Btn :: PnlElem PnlBtnMod
         Pnl_CurrPrefNameLbl :: PnlElem NoMod
         Pnl_Header :: PnlElem NoMod
         Pnl_PrefMenuLbl :: PnlElem NoMod
         Pnl_Sect :: PnlElem PnlSectMod
         Pnl_SectItemList :: PnlElem NoMod
         Pnl_SectLbl :: PnlElem NoMod
         Pnl_TextInput :: PnlElem NoMod
deriving instance Show (PnlElem a)

data PnlBtnMod = PnlBtn_Size Size | PnlBtn_Vert deriving Show

data Size = Big deriving Show

data PnlSectMod = PnlSect_Blked deriving Show

data RootElem a where
         Root_Pnl :: RootElem NoMod
deriving instance Show (RootElem a)



data Blk' (a :: Type -> Type) b where
         Btn' :: Blk' NoElem BtnMod'
         Header :: Blk' HeaderElem HeaderMod
         Logo :: Blk' NoElem NoMod
         Search :: Blk' SearchElem SearchMod
         TextInput :: Blk' NoElem NoMod
deriving instance Show (Blk' a b)

data BtnMod' = Btn_Dark deriving Show

data HeaderMod = Header_Dark

data HeaderElem a where
         Header_Logo :: HeaderElem NoMod
         Header_Search :: HeaderElem NoMod
deriving instance Show (HeaderElem a)

data SearchBtnMod = SearchBtn_Size Size' deriving Show

data SearchElem a where
         Search_Btn :: SearchElem SearchBtnMod
deriving instance Show (SearchElem a)

data SearchMod = Search_Dark

data Size' = Big' | Small deriving Show

data TextInputMod = TextInput_Dark
