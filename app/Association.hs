{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Association where

import           Data.Text        (Text, pack)
import           GHC.Generics

data Association = Association
    { firstUserId   :: Int
    , secondUserId  :: Int
    , cost          :: Int
    } deriving (Generic, Show)