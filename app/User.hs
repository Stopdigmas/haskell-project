{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module User where

import           Data.Text        (Text, pack)
import           GHC.Generics

data User = User
    { userName :: Text
    , avatarUrl :: Text
    } deriving (Generic, Show)