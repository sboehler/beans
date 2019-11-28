module Env where

import Database (Connection)
import Lens.Micro.Platform (makeFields)

data Env
  = Env
      { _envPool :: Connection
      }

makeFields ''Env
