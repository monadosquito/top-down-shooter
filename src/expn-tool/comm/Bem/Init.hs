{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE GADTs #-}


module Bem.Init
    ( module Bem.Blk
    , module Bem.Init
    , module Bem.Miso.View.Cfg
    ) where


import Act
import Bem.Blk
import Mdl

import Bem.Cfg.Cfg
import Bem.Miso.View.Cfg
import qualified Bem.Miso.Utl.Utl as Bem
import qualified Bem.Miso.View.Cfg as Bem


viewGens :: Bem.Mks Act' Mdl
viewGens
    =
    Bem.init defCfg { _partSep = "AAA"
                    }


-- type MkBlk isAlsoDomAct trigGrpCrit = Bem.MkBlk (Act Double isAlsoDomAct trigGrpCrit) (Mdl trigGrpCrit) Blk
-- type MkNoBlkModsBlk isAlsoDomAct trigGrpCrit = Bem.MkNoBlkModsBlk (Act Double isAlsoDomAct trigGrpCrit) (Mdl trigGrpCrit) Blk
-- type MkNoElemModsBlk isAlsoDomAct trigGrpCrit = Bem.MkNoElemModsBlk (Act Double isAlsoDomAct trigGrpCrit) (Mdl trigGrpCrit) Blk
-- type MkNoModsBlk isAlsoDomAct trigGrpCrit = Bem.MkNoModsBlk (Act Double isAlsoDomAct trigGrpCrit) (Mdl trigGrpCrit) Blk
-- 
-- type Elem isAlsoDomAct trigGrpCritType = Bem.Elem (Act Double isAlsoDomAct trigGrpCritType) Blk
-- type NoModsElem isAlsoDomAct trigGrpCrit = Bem.NoModsElem (Act Double isAlsoDomAct trigGrpCrit) Blk
-- 
-- type Mix isAlsoDomAct trigGrpCrit = Bem.Mix (Act Double isAlsoDomAct trigGrpCrit) Blk
-- type NoBlkModsMix isAlsoDomAct trigGrpCrit = Bem.NoBlkModsMix (Act Double isAlsoDomAct trigGrpCrit) Blk
-- type NoElemModsMix isAlsoDomAct trigGrpCrit = Bem.NoElemModsMix (Act Double isAlsoDomAct trigGrpCrit) Blk
-- type NoModsMix isAlsoDomAct trigGrpCrit = Bem.NoModsMix (Act Double isAlsoDomAct trigGrpCrit) Blk

type MkBlk = Bem.MkBlk Act' Mdl
-- type MkNoBlkModsBlk isAlsoDomAct trigGrpCrit = Bem.MkNoBlkModsBlk (Act Double isAlsoDomAct) Mdl Blk
-- type MkNoElemModsBlk isAlsoDomAct trigGrpCrit = Bem.MkNoElemModsBlk (Act Double isAlsoDomAct) Mdl Blk
type MkNoModsBlk = Bem.MkNoModsBlk Act' Mdl
-- 
type Elem = Bem.Elem Act'
type NoModsElem = Bem.NoModsElem Act'
-- 
-- type Mix isAlsoDomAct trigGrpCrit = Bem.Mix (Act Double isAlsoDomAct) Blk
-- type NoBlkModsMix isAlsoDomAct trigGrpCrit = Bem.NoBlkModsMix (Act Double isAlsoDomAct) Blk
-- type NoElemModsMix isAlsoDomAct trigGrpCrit = Bem.NoElemModsMix (Act Double isAlsoDomAct) Blk
-- type NoModsMix isAlsoDomAct trigGrpCrit = Bem.NoModsMix (Act Double isAlsoDomAct) Blk
