module Dtl.Upd.Cold.WithEff where


import Dtl.Act.Cold.WithEff
import Dtl.Mdl.Cold

import Miso


upd :: Act -> Mdl -> Effect () Mdl
upd ColdWithEff mdl = mdl <# return ()
