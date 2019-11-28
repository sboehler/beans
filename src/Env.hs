module Env where

import Config (Config)
import Lens.Micro.Platform (makeFields)

-- type ConnectionPool = P.Pool PG.Connection

data Env
  = Env
      { _envConfig :: Config
      }

makeFields ''Env
