module Capabilities.Database
  ( initializeDatabase
  , PGS.Connection
  , createPool
  , Database (..)
  )
where

import Config
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Pool as P
import Data.Text (unpack)
import Database.Beam.Backend.SQL.Row (FromBackendRow)
import Database.Beam.Postgres (ConnectInfo (..), Postgres)
import qualified Database.Beam.Postgres.Conduit as C
import Database.Beam.Query (SqlSelect)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Migration as PGS
import Env
import RIO
import RIO.Orphans ()

createPool :: MonadIO m => Config -> m (P.Pool PGS.Connection)
createPool c = do
  let open =
        PGS.connect
          PGS.defaultConnectInfo
            { connectHost = unpack $ c ^. databaseHost
            , connectPort = fromIntegral $ c ^. databasePort
            , connectUser = unpack $ c ^. databaseUser
            , connectPassword = unpack $ c ^. databasePassword
            , connectDatabase = unpack $ c ^. databaseName
            }
  liftIO $ P.createPool open PGS.close 1 10 10

initializeDatabase :: MonadIO m => FilePath -> PGS.Connection -> m ()
initializeDatabase dir con = do
  liftIO $
    PGS.withTransaction con $ do
    res <- PGS.runMigrations True con [PGS.MigrationInitialization, PGS.MigrationDirectory dir]
    case res of
      PGS.MigrationError e -> error e
      _ -> return ()

--------------------------------------------------------------------------------
class (Monad m) => Database m where

  runSelect :: (FromBackendRow Postgres a) => SqlSelect Postgres a -> (C.ConduitT () a m () -> m b) -> m b

  runSelectMany :: (FromBackendRow Postgres a) => SqlSelect Postgres a -> m [a]
  runSelectMany q = runSelect q (\c -> C.runConduit (c C..| C.consume))

  runSelectMaybe :: (FromBackendRow Postgres a) => SqlSelect Postgres a -> m (Maybe a)
  runSelectMaybe q = runSelect q (\c -> C.runConduit (c C..| C.await))

  runSelectOne :: (FromBackendRow Postgres a) => SqlSelect Postgres a -> m a
  runSelectOne q = runSelectMaybe q >>= maybe (error "error") return

--------------------------------------------------------------------------------
instance Database (RIO Env) where

  runSelect q f = do
    con <- view connection
    C.runSelect con q f
