{-# LANGUAGE UndecidableInstances #-}

module Capabilities.Database
  ( initializeDatabase
  , PG.Connection
  , createPool
  , Database(..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Pool as P
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Migration as PG
import Env
import RIO

createPool :: MonadIO m => m (P.Pool PG.Connection)
createPool = liftIO $ P.createPool open PG.close 1 10 10
  where
    open = PG.connectPostgreSQL "dbname=webapp"

initializeDatabase :: MonadIO m => FilePath -> PG.Connection -> m ()
initializeDatabase dir con =
  liftIO $
  PG.withTransaction con $
  mapM_ migrate [PG.MigrationInitialization, PG.MigrationDirectory dir]
  where
    migrate c = PG.runMigration $ PG.MigrationContext c True con

class Monad m =>
      Database m
  where
  fetch1_ :: PG.FromRow b => PG.Query -> m (Maybe b)
  fetch1 :: (PG.FromRow b, PG.ToRow c) => PG.Query -> c -> m (Maybe b)
  fetchN_ :: (PG.FromRow b) => PG.Query -> m [b]
  fetchN :: (PG.FromRow b, PG.ToRow c) => PG.Query -> c -> m [b]

instance HasConnection a PG.Connection => Database (RIO a) where
  fetch1_ q = do
    con <- view connection
    listToMaybe <$> liftIO (PG.query_ con q)
  fetch1 q args = do
    con <- view connection
    listToMaybe <$> liftIO (PG.query con q args)
  fetchN_ q = do
    con <- view connection
    liftIO $ PG.query_ con q
  fetchN q args = do
    con <- view connection
    liftIO $ PG.query con q args
