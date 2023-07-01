module Dtl.Upd.Warm.NoEff where


import Dtl.Act.Warm.NoEff
import Dtl.Mdl.Warm

import Miso


upd :: Act -> Mdl -> Mdl
upd WarmAct mdl = mdl
