{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GithubRequisition where

import Prelude.Compat
import qualified Data.Vector
import Data.Text         (Text, pack, unpack)
import qualified Data.Text.IO as T (putStrLn)
import Data.Monoid       ((<>))

import qualified GitHub.Endpoints.Repos as GitHub
import Data.List
import Data.Maybe
import qualified GitHub.Data.Name as Name

getGithubRepos = do
    possibleRepos <- GitHub.organizationRepos "fga-eps-mds"
    case possibleRepos of
        (Left error)  -> putStrLn $ "Error: " ++ (show error)
        (Right repos) -> putStrLn $ intercalate "\n\n" $ map formatRepo (Data.Vector.toList repos)

formatRepo repo =
    (Data.Text.unpack (Name.untagName (GitHub.repoName repo)))


