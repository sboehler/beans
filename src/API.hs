module API
  ( API
  , api
  )
where

import API.Health (HealthAPI, healthAPI)
import qualified Capabilities.Crypto as C
import qualified Capabilities.Database as D
import RIO
import Servant ((:>), ServerT)

type API
  = "api" :> HealthAPI

api :: (C.Crypto m, MonadThrow m, D.Database m) => ServerT API m
api = healthAPI
