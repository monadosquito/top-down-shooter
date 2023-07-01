module Dtl.Mdl.Comm where


data Mdl = Mdl {commFld :: Bool} deriving Eq


init :: Mdl
init = Mdl {commFld = False}
