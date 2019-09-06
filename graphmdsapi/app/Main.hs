{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Web.Spock -- web framework
import           Web.Spock.Config

import           Data.Aeson       hiding (json) -- module to work with json data 
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics

