module Dtl.Mdl.Cold where


data Mdl = Mdl {coldFld :: Bool} deriving Eq


init :: Mdl
init = Mdl {coldFld = True}
