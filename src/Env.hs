module Env where

import Config (Config)
import qualified Data.Pool as P
import qualified Database.PostgreSQL.Simple as PG
import Lens.Micro.Platform (makeFields)
import Servant.Auth.Server (CookieSettings, JWTSettings)

type ConnectionPool = P.Pool PG.Connection

data Env
  = Env
      { _envConfig :: Config
      , _envCookieSettings :: CookieSettings
      , _envJwtSettings :: JWTSettings
      , _envConnection :: PG.Connection
      }

makeFields ''Env
