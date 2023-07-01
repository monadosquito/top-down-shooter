{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}


module Core.Configuration where


import Data.Kind


type EnabledFlag :: Flag -> Constraint
class EnabledFlag flag

type EnabledOption :: OptionKey -> OptionValue -> Constraint
class EnabledOption optionKey optionValue


data Flag = NeedComparison | NeedSerialization

data OptionKey

data OptionValue
-- #ifdef DEV
instance EnabledFlag 'NeedComparison
instance EnabledFlag 'NeedSerialization
-- #endif
