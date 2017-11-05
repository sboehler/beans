module Capabilities.Database
  ( initializeDatabase
  , PG.Connection
  , createPool
  , Database(..)
  , fetch1
  , fetch1_
  , fetchN
  , fetchN_
  , runDatabase
  ) where

import Control.Monad.Freer hiding (run)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 as BS8 (pack)
import Data.Maybe (listToMaybe)
import qualified Data.Pool as P
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Migration as PG
import Servant (Handler)

createPool :: IO (P.Pool PG.Connection)
createPool = P.createPool open PG.close 1 10 10
  where
    open = PG.connectPostgreSQL (BS8.pack url)
    url = "dbname=webapp"

initializeDatabase :: FilePath -> PG.Connection -> IO ()
initializeDatabase dir con =
  PG.withTransaction con $
  mapM_ migrate [PG.MigrationInitialization, PG.MigrationDirectory dir]
  where
    migrate c = PG.runMigration $ PG.MigrationContext c True con

data Database a where
  Fetch1_ :: PG.FromRow b => PG.Query -> Database (Maybe b)
  Fetch1 :: (PG.FromRow b, PG.ToRow c) => PG.Query -> c -> Database (Maybe b)
  FetchN_ :: PG.FromRow b => PG.Query -> Database [b]
  FetchN :: (PG.FromRow b, PG.ToRow c) => PG.Query -> c -> Database [b]

fetch1_ ::
     (Member Database effs, PG.FromRow b) => PG.Query -> Eff effs (Maybe b)
fetch1_ = send . Fetch1_

fetch1 ::
     (Member Database effs, PG.FromRow b, PG.ToRow c)
  => PG.Query
  -> c
  -> Eff effs (Maybe b)
fetch1 q = send . Fetch1 q

fetchN_ :: (Member Database effs, PG.FromRow b) => PG.Query -> Eff effs [b]
fetchN_ = send . FetchN_

fetchN ::
     (Member Database effs, PG.FromRow b, PG.ToRow c)
  => PG.Query
  -> c
  -> Eff effs [b]
fetchN q = send . FetchN q

runDatabase ::
     forall a effs. (LastMember Handler effs, Member Handler effs)
  => PG.Connection
  -> Eff (Database ': effs) a
  -> Eff effs a
runDatabase con =
  interpretM $ \case
    Fetch1_ q -> listToMaybe <$> liftIO (PG.query_ con q)
    Fetch1 q args -> listToMaybe <$> liftIO (PG.query con q args)
    FetchN_ q -> liftIO $ PG.query_ con q
    FetchN q args -> liftIO $ PG.query con q args
