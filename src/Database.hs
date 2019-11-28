module Database
  ( createPool,
    ConnectionPool,
    P.withResource,
    Connection,
  )
where

import qualified Data.Pool as P
import Database.SQLite.Simple (Connection, close, open)
import RIO

type ConnectionPool = P.Pool Connection

createPool :: FilePath -> IO (ConnectionPool)
createPool dbfile = P.createPool (open dbfile) close 1 0.5 10
