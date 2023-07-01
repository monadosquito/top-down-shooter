module Dtl.Upd.Comm.WithEff where


import Dtl.Act.Comm.WithEff
import Dtl.Mdl.Comm

import Miso


upd :: Act -> Mdl -> Effect () Mdl
upd CommWithEff mdl = mdl <# return ()
