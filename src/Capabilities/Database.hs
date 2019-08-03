{-# LANGUAGE UndecidableInstances #-}

module Capabilities.Database
  ( initializeDatabase
  , PGS.Connection
  , createPool
  , Database (..)
  )
where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Pool as P
import Database.Beam.Backend.SQL.Row (FromBackendRow)
import Database.Beam.Postgres (Postgres)
import qualified Database.Beam.Postgres.Conduit as C
import Database.Beam.Query (SqlSelect)
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
class (Monad m, Monad n) => Database m n | m -> n where

  initialize :: FilePath -> m ()

  runSelect :: (FromBackendRow Postgres a) => SqlSelect Postgres a -> (C.ConduitT () a n () -> n b) -> m b

  runSelectMany :: (FromBackendRow Postgres a) => SqlSelect Postgres a -> m [a]
  runSelectMany q = runSelect q (\c -> C.runConduit (c C..| C.consume))

  runSelectMaybe :: (FromBackendRow Postgres a) => SqlSelect Postgres a -> m (Maybe a)
  runSelectMaybe q = runSelect q (\c -> C.runConduit (c C..| C.await))

  runSelectOne :: (FromBackendRow Postgres a) => SqlSelect Postgres a -> m a
  runSelectOne q = runSelectMaybe q >>= maybe (error "error") return

--------------------------------------------------------------------------------
instance HasConnection a PGS.Connection => Database (RIO a) IO where

  initialize dir = do
    con <- view connection
    let migrate c = PGS.runMigration $ PGS.MigrationContext c True con
    liftIO $ PGS.withTransaction con $
      mapM_ migrate [PGS.MigrationInitialization, PGS.MigrationDirectory dir]

  runSelect q f = do
    con <- view connection
    C.runConduit $ liftIO $ C.runSelect con q f
