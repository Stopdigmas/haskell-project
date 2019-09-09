{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GithubRequisitionUsers where

import Prelude.Compat

import Data.Text         (Text, pack)
import Data.Text.IO as T (putStrLn)
import Data.Monoid       ((<>))
import qualified GitHub.Endpoints.Repos.Collaborators as GitHub

getGithubUsers :: IO ()
getGithubUsers = do
    possibleCollaborators <- GitHub.collaboratorsOn "fga-eps-mds" "2018.2-Kalkuli"
    T.putStrLn $ either (("Error: " <>) . pack . show)
                        (foldMap ((<> "\n") . formatUser))
                        possibleCollaborators

formatUser :: GitHub.SimpleUser -> Text
formatUser = GitHub.untagName . GitHub.simpleUserLogin