{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Schema where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource
import Database.Persist.TH
import Data.Time.Clock
import Control.Monad.Trans.Reader
import Data.Text (pack)
import Control.Monad

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Person
    name String
    age Int
    createdAt UTCTime
    deriving Show
|]

createSchema :: FilePath -> IO ()
createSchema fileDB = runSqlite (pack fileDB) $ do runMigration migrateAll

runQueries :: FilePath
           -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
           -> IO ()
runQueries fileDB trans =
  runSqlite (pack fileDB) $
  do runMigration migrateAll
     trans

deleteAllRows
  :: MonadIO m
  => ReaderT SqlBackend m ()
deleteAllRows = rawExecute "delete from person" []

bulkInsert
  :: MonadIO m
  => ReaderT SqlBackend m ()
bulkInsert = do
  currentTime <- liftIO $ getCurrentTime
  forM_ [1 .. 10000] (\_ -> insert $ Person "Sibi" 27 currentTime)

initialize :: FilePath -> IO ()
initialize fileDB = do
  createSchema fileDB
  runQueries fileDB bulkInsert

cleanup :: FilePath -> IO ()
cleanup fileDB = runQueries fileDB deleteAllRows

mockSchema :: IO ()
mockSchema = mockMigration migrateAll
