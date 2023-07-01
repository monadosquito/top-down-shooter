module Dtl.Upd.Warm.WithEff where


import Dtl.Act.Warm.WithEff
import Dtl.Mdl.Warm

import Miso


upd :: Act -> Mdl -> Effect () Mdl
upd WarmWithEff mdl = mdl <# (return ())
