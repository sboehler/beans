module API.Health
  ( HealthAPI,
    healthAPI,
  )
where

import RIO
import Servant
  ( (:>),
    Get,
    PlainText,
    ServerT,
  )

--------------------------------------------------------------------------------
type GetHealthR =
  "health"
    :> Get '[PlainText] Text

getHealth :: (MonadIO m, MonadReader env m, HasLogFunc env) => ServerT GetHealthR m
getHealth = do
  logInfo "Hello!"
  pure "Healthy!\n"

--------------------------------------------------------------------------------
type HealthAPI = GetHealthR

healthAPI :: (MonadIO m, MonadReader env m, HasLogFunc env) => ServerT HealthAPI m
healthAPI = getHealth
