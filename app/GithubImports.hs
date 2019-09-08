{-# LANGUAGE OverloadedStrings #-}

module GithubImports where

import           Network
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Header
import           Data.Text (Text)
import qualified Data.ByteString.Char8 as S8
import           Data.Aeson
import           GHC.Generics

getRepositories :: IO String
getRepositories = do

  initReq <- parseRequest "https://api.github.com/orgs/fga-eps-mds/repos?page=1"

  let r = initReq
                { method = "GET"
                , requestHeaders = [(hAuthorization, "token bf0f4b083f1fe8b03a2a652796725b5d55fa9a6c")
                                  , ("User-Agent", "Stopdigmas")]}
  
  let request = setQueryString [("direction", Just "desc")] r

  manager <- newManager tlsManagerSettings
  res <- httpLbs request manager
  return . show . responseBody $ res
