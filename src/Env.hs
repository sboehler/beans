module Env where

import Database (Connection)
import Lens.Micro.Platform (makeFields)
import RIO

data Env
  = Env
      { _envPool :: Connection
      ,  _envLogFunction :: LogFunc
      }

makeFields ''Env

instance HasLogFunc Env where
  logFuncL = logFunction
