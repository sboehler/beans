module Env where

import qualified Data.Pool as P
import qualified Database.PostgreSQL.Simple as PG
import Lens.Micro.Platform (makeFields)
import Servant.Auth.Server (CookieSettings, JWTSettings)

type ConnectionPool = P.Pool PG.Connection

data Env = Env
  { _envConnection :: PG.Connection
  , _envCookieSettings :: CookieSettings
  , _envJwtSettings :: JWTSettings
  }

makeFields ''Env
