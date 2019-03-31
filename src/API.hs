module API
  ( API
  , api
  ) where

import API.Health (HealthAPI, healthAPI)
import API.User (UserAPI, userAPI)
import Env
import RIO
import Servant ((:<|>)(..), ServerT)

type API
   = UserAPI
     :<|> HealthAPI

api :: ServerT API (RIO Env)
api = userAPI :<|> healthAPI
