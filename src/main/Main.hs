{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}


#ifdef DEV
import WwwEnv
#endif
import Glue
import qualified Dtl.Act.Comm.NoEff as CommNoEff
import qualified Dtl.Mdl.Comm as Comm
#ifdef COLD_PLATF
import qualified Dtl.Mdl.Cold as Spcl
#elif defined(WARM_PLATF)
import qualified Dtl.Mdl.Warm as Spcl
#endif
import qualified Miso
#ifdef DEV
app :: Miso.JSM ()
app
#else
main :: Miso.JSM ()
main
#endif
    = Miso.startApp misoApp
  where
#ifdef DEV
    _ = inclPub
    _ = loadPub
#endif
    misoApp = Miso.App
                  { Miso.events = Miso.defaultEvents
                  , Miso.initialAction = CommNoEff CommNoEff.Pass
                  , Miso.logLevel = Miso.DebugPrerender
                  , Miso.model = (Comm.init, Spcl.init)
                  , Miso.mountPoint = Nothing
                  , Miso.subs = []
                  , Miso.update = upd
                  , Miso.view = \_ -> Miso.h1_ [] [Miso.text "Main application"]
                  }
