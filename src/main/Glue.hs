{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}


module Glue where


import qualified Dtl.Act.Comm.Eff as CommEff
import qualified Dtl.Act.Comm.NoEff as CommNoEff
import qualified Dtl.Act.Comm.WithEff as CommWithEff
import qualified Dtl.Mdl.Comm as CommNoEff
import qualified Dtl.Upd.Comm.Eff as CommEff
import qualified Dtl.Upd.Comm.NoEff as CommNoEff
import qualified Dtl.Upd.Comm.WithEff as CommWithEff
#ifdef COLD_PLATF
import qualified Dtl.Act.Cold.Eff as SpclEff
import qualified Dtl.Act.Cold.NoEff as SpclNoEff
import qualified Dtl.Act.Cold.WithEff as SpclWithEff
import qualified Dtl.Mdl.Cold as SpclNoEff
import qualified Dtl.Upd.Cold.Eff as SpclEff
import qualified Dtl.Upd.Cold.NoEff as SpclNoEff
import qualified Dtl.Upd.Cold.WithEff as SpclWithEff
#elif defined(WARM_PLATF)
import qualified Dtl.Act.Warm.Eff as SpclEff
import qualified Dtl.Act.Warm.NoEff as SpclNoEff
import qualified Dtl.Act.Warm.WithEff as SpclWithEff
import qualified Dtl.Mdl.Warm as SpclNoEff
import qualified Dtl.Upd.Warm.Eff as SpclEff
import qualified Dtl.Upd.Warm.NoEff as SpclNoEff
import qualified Dtl.Upd.Warm.WithEff as SpclWithEff
#endif
import Miso


data Act
    = CommEff CommEff.Act
    | CommNoEff CommNoEff.Act
    | CommWithEff CommWithEff.Act
    | SpclEff SpclEff.Act
    | SpclNoEff SpclNoEff.Act
    | SpclWithEff SpclWithEff.Act


upd
    :: Act
    -> (CommNoEff.Mdl, SpclNoEff.Mdl)
    -> Effect Act (CommNoEff.Mdl, SpclNoEff.Mdl)
upd (CommEff act) mdl@(comm, _)
    =
    mdl <# (CommEff.upd act comm >> return (CommNoEff CommNoEff.Pass))
upd (CommNoEff act) (comm, spcl) = noEff (CommNoEff.upd act comm, spcl)
upd (CommWithEff act) (comm, spcl)
    =
    Effect mdl $ map (mapSub $ CommNoEff . const CommNoEff.Pass) subs'
  where
    Effect mdl subs' = (, spcl) <$> CommWithEff.upd act comm
upd (SpclEff act) mdl@(_, spcl)
    =
    mdl <# (SpclEff.upd act spcl >> return (CommNoEff CommNoEff.Pass))
upd (SpclNoEff act) (comm, spcl) = noEff (comm, SpclNoEff.upd act spcl)
upd (SpclWithEff act) (comm, spcl)
    =
    Effect mdl (map (mapSub $ CommNoEff . const CommNoEff.Pass) subs')
  where
    Effect mdl subs' = (comm, ) <$> SpclWithEff.upd act spcl
