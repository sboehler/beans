module API
  ( API,
    api,
  )
where

import API.Health (HealthAPI, healthAPI)
import RIO
import Servant
  ( (:>),
    ServerT,
  )

type API =
  "api" :> (HealthAPI)

api :: (MonadReader env m, HasLogFunc env, MonadIO m, MonadThrow m) => ServerT API m
api = healthAPI
