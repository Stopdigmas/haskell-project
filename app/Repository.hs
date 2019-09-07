{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Repository where

import           Data.Text        (Text, pack)
import           GHC.Generics

data Repository = Repository
    { repName :: Text
    } deriving (Generic, Show)