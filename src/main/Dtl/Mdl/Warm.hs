module Dtl.Mdl.Warm where


data Mdl = Mdl {warmFld :: Bool} deriving Eq


init :: Mdl
init = Mdl {warmFld = True}
