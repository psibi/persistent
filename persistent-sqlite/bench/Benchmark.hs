{-# LANGUAGE ScopedTypeVariables #-}

module Benchmark where

import Schema
import Data.Monoid ((<>))
import Gauge.Main
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader
import Database.Persist.Sqlite
import Data.List (foldl')

checkTotal :: Int -> IO ()
checkTotal tot = do
  let total = 27 * 10000
  if (total == tot)
    then return ()
    else fail $ "Invalid total: Expected " <> (show total) <> "Got " <> show tot

selectAge
  :: MonadIO m
  => ReaderT SqlBackend m ()
selectAge = do
  (rows :: [Entity Person]) <- selectList [] []
  liftIO $ checkTotal $ foldl' (\acc x -> personAge (entityVal x) + acc) 0 rows

sampleDB :: FilePath
sampleDB = "sample.db"

selectBenchmark :: Benchmark
selectBenchmark =
  envWithCleanup
    (initialize sampleDB)
    (const (cleanup sampleDB))
    (const (bench "selectList" $ whnfIO $ runQueries sampleDB selectAge))

runBenchmarks :: IO ()
runBenchmarks = defaultMain [bgroup "selectList" [selectBenchmark]]
