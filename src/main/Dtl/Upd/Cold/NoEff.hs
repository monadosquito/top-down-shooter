module Dtl.Upd.Cold.NoEff where


import Dtl.Act.Cold.NoEff
import Dtl.Mdl.Cold


upd :: Act -> Mdl -> Mdl
upd ColdAct mdl = mdl
