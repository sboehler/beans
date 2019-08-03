{-# LANGUAGE UndecidableInstances #-}

module Capabilities.Database
  ( initializeDatabase
  , PGS.Connection
  , createPool
  , Database (..)
  )
where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Pool as P
import Database.Beam.Postgres as BPG
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Migration as PGS
import Env
import RIO

createPool :: MonadIO m => m (P.Pool PGS.Connection)
createPool = liftIO $ P.createPool open PGS.close 1 10 10
  where
    open = PGS.connectPostgreSQL "dbname=dev host=localhost user=dev password=dev port=15432"

initializeDatabase :: MonadIO m => FilePath -> PGS.Connection -> m ()
initializeDatabase dir con = do
  liftIO $
    PGS.withTransaction con $ do
    res <- PGS.runMigrations True con [PGS.MigrationInitialization, PGS.MigrationDirectory dir]
    case res of
      PGS.MigrationError e -> error e
      _ -> return ()

--------------------------------------------------------------------------------
class (MonadIO m) => Database m where

  initialize :: FilePath -> m ()

  runStatement :: BPG.Pg a -> m a

  getConnection :: m Connection

  fetch1_ :: PGS.FromRow b => PGS.Query -> m (Maybe b)

  fetch1 :: (PGS.FromRow b, PGS.ToRow c) => PGS.Query -> c -> m (Maybe b)

  fetchN_ :: (PGS.FromRow b) => PGS.Query -> m [b]

  fetchN :: (PGS.FromRow b, PGS.ToRow c) => PGS.Query -> c -> m [b]

--------------------------------------------------------------------------------
instance HasConnection a PGS.Connection => Database (RIO a) where

  initialize dir = do
    con <- view connection
    let migrate c = PGS.runMigration $ PGS.MigrationContext c True con
    liftIO $ PGS.withTransaction con $
      mapM_ migrate [PGS.MigrationInitialization, PGS.MigrationDirectory dir]

  runStatement q = do
    con <- view connection
    liftIO $ BPG.runBeamPostgres con q

  getConnection = view connection

  fetch1_ q = do
    con <- view connection
    listToMaybe <$> liftIO (PGS.query_ con q)

  fetch1 q args = do
    con <- view connection
    listToMaybe <$> liftIO (PGS.query con q args)

  fetchN_ q = do
    con <- view connection
    liftIO $ PGS.query_ con q

  fetchN q args = do
    con <- view connection
    liftIO $ PGS.query con q args
