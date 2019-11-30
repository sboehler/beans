module Database
  ( createPool,
    ConnectionPool,
    P.withResource,
    S.Connection,
    initialize,
  )
where

import qualified Data.Pool as P
import qualified Database.SQLite.Simple as S
import qualified Database.SQLite.Simple.FromField as S
import RIO
import Prelude (putStrLn)

type Connection = S.Connection

type ConnectionPool = P.Pool Connection

createPool :: FilePath -> IO (ConnectionPool)
createPool dbfile = P.createPool (S.open dbfile) S.close 1 0.5 10

initialize :: FilePath -> IO ()
initialize dbfile =
  S.withConnection dbfile migrate

migrate :: S.Connection -> IO ()
migrate con = do
  S.execute_
    con
    "create table if not exists schema_version (version int not null); \
    \create unique index if not exists schema_version_unique on schema_version (version);"
  version <- fromMaybe 0 <$> getOne con "select max(version) from schema_version" :: IO Int64
  let steps = filter ((> version) . fst) migrations
  putStrLn $ "Current Schema Version: " ++ show version
  forM_ steps $ \(step, query) -> do
    putStrLn $ "Migrating to version: " ++ show step
    S.withTransaction con $ do
      S.execute_ con query
      S.execute con "insert into schema_version (version) values (?)" (S.Only step)
  return ()

getOne :: S.FromField a => S.Connection -> S.Query -> IO a
getOne con query = do
  [S.Only a] <- S.query_ con query
  return a

migrations :: [(Int64, S.Query)]
migrations =
  [ (1, "create table test (test text not null)")
  ]
