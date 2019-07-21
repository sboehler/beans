module API.Health
  ( HealthAPI
  , healthAPI
  )
where

import RIO
import Servant ((:>), Get, PlainText, ServerT)

--------------------------------------------------------------------------------
type GetHealthR
  = "health"
    :> Get '[PlainText] Text

getHealth :: Monad m => m Text
getHealth = pure "Healthy!"

--------------------------------------------------------------------------------
type HealthAPI = GetHealthR

healthAPI :: Monad m => ServerT HealthAPI m
healthAPI = getHealth
