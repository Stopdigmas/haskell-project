{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (get) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist        as P         -- We'll be using P.get later for GET /people/<id>.
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH

import           User
import           Repository
import           Association

-- type Api = SpockM SqlBackend () () ()

-- type ApiAction a = SpockAction SqlBackend () () a

-- main :: IO ()
-- main = do
--     pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
--     spockCfg <- defaultSpockCfg () (PCPool pool) ()
--     runStdoutLoggingT $ runSqlPool (do 
--                                     runMigration User.migrateAll 
--                                     runMigration Repository.migrateAll
--                                     runMigration Association.migrateAll) pool
--     runSpock 8080 (spock spockCfg app)

-- runSQL
--     :: (HasSpock m, SpockConn m ~ SqlBackend)
--     => SqlPersistT (LoggingT IO) a -> m a
-- runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn


-- errorJson :: Int -> Text -> ApiAction ()
-- errorJson code message =
--     json $
--         object
--         [ "result" .= String "failure"
--         , "error" .= object ["code" .= code, "message" .= message]
--         ]

-- app :: Api
-- app = do
--     get "users" $ do
--         allUsers <- runSQL $ selectList [] [Asc UserId]
--         json allUsers

--     post "users" $ do
--         maybeUser <- jsonBody :: ApiAction (Maybe User)
--         case maybeUser of
--             Nothing -> errorJson 1 "Failed to parse request body as User"
--             Just theUser -> do
--                 newId <- runSQL $ insert theUser
--                 json $ object ["result" .= String "success", "id" .= newId]

--     get ("users" <//> var) $ \userName -> do
--         maybeUser <- runSQL $ P.get userName :: ApiAction (Maybe User)
--         case maybeUser of
--             Nothing -> errorJson 2 "Could not find a user with matching id"
--             Just theUser -> json theUser
    
--     get "repositories" $ do
--         allRepos <- runSQL $ selectList [] [Asc RepositoryId]
--         json allRepos

--     post "repositories" $ do
--         maybeRepo <- jsonBody :: ApiAction (Maybe Repository)
--         case maybeRepo of
--             Nothing -> errorJson 1 "Failed to parse request body as Repository"
--             Just theRepo -> do
--                 newId <- runSQL $ insert theRepo
--                 json $ object ["result" .= String "success", "id" .= newId]
