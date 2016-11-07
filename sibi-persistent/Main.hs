{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.MongoDB
import Database.MongoDB.Admin
import Database.MongoDB hiding (insert)
import Control.Monad.Cont
import MongoImport
import Network (PortID(PortNumber))
import Language.Haskell.TH (Type(..))

share
    [mkPersist mongoSettings]
    [persistLowerCase|
User
    name String
    age Int
    UniqueAge age
    deriving Show
|]

runDBActions actions =
    withMongoDBConn
        "myDatabaseName"
        "localhost"
        (PortNumber 27017)
        Nothing
        2000 $
    \pool ->
         runMongoDBPool master actions pool

-- upsert an existing row already works
-- it "updates an existing row" $ db $ do
--     initial <- insertEntity (Upsert "a" "initial" "extra")
--     update' <-
--         upsert (Upsert "a" "wow" "such unused") [UpsertAttr =. "update"]
--     ((==@) `on` entityKey) initial update'
--     upsertAttr (entityVal update') @== "update"
--     upsertExtra (entityVal update') @== "extra"
ageIndex =
    Index
    { iColl = "user"
    , iKey = ["age" =. 1]
    , iName = "userAgeUnique"
    , iUnique = True
    , iDropDups = False
    , iExpireAfterSeconds = Nothing
    }

actions = do
    a <- insert (User "sibi" 34)
    createIndex ageIndex
    return ()

-- actions = do
--     u <- insert $ User "John Doe" $ Just 35
--     ent <- insertEntity $ (Blogpost "my first post" u)
--     let newBg = (Blogpost "jassos" u)
--     let upds = [BlogpostTitle =. "fasoos"]
--     x <- upsert newBg []
--     liftIO $ print (entityVal ent)
--     liftIO $ print (entityVal x)
--     return ()
main
    :: IO ()
main = do
    runDBActions actions-- import Control.Monad.IO.Class (liftIO)
                        -- import Database.Persist
                        -- -- import Database.Persist.Sqlite
                        -- import Database.Persist.Postgresql
                        -- import Database.Persist.TH
                        -- import Control.Monad.Logger
                        -- share
                        --     [mkPersist sqlSettings, mkMigrate "migrateAll"]
                        --     [persistLowerCase|
                        -- Usr
                        -- Account
                        --     usr UsrId
                        --     balance Int
                        --     UniqueAccount usr
                        --     deriving Show
                        -- |]
                        -- main :: IO ()
                        -- main = putStrLn "hello world" >> insertBug >> updateBug
                        -- connStr =
                        --     "host=localhost dbname=test1 user=postgres password=postgres port=5432"
                        -- insertBug =
                        --     runStderrLoggingT $
                        --     withPostgresqlPool connStr 10 $
                        --     \pool ->
                        --          liftIO $
                        --          do flip runSqlPersistMPool pool $
                        --                 do do runMigration migrateAll
                        --                       u <- insert Usr
                        --                       upsert (Account u 2) [AccountBalance +=. 2]
                        --                       liftIO (putStr "Should be 2: ")
                        --                       liftIO . print =<<
                        --                           map (accountBalance . entityVal) <$> selectList [] []
                        -- updateBug =
                        --     runStderrLoggingT $
                        --     withPostgresqlPool connStr 10 $
                        --     liftSqlPersistMPool $
                        --     do do runMigration migrateAll
                        --           u <- insert Usr
                        --           insert (Account u 0)
                        --           upsert (Account u 100) []
                        --           liftIO (putStr "Should be 0: ")
                        --           liftIO . print =<<
                        --               map (accountBalance . entityVal) <$> selectList [] []