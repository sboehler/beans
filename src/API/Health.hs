module API.Health
  ( HealthAPI
  , healthAPI
  ) where

import Env
import RIO
import Servant ((:>), Get, PlainText, ServerT)

--------------------------------------------------------------------------------
type GetHealthR
   = "health"
     :> Get '[ PlainText] Text

getHealth :: RIO Env Text
getHealth = pure "Healthy!"

--------------------------------------------------------------------------------
type HealthAPI = GetHealthR

healthAPI :: ServerT HealthAPI (RIO Env)
healthAPI = getHealth
