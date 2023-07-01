module Dtl.Upd.Comm.NoEff where


import Dtl.Act.Comm.NoEff
import Dtl.Mdl.Comm


upd :: Act -> Mdl -> Mdl
upd Pass mdl = mdl
