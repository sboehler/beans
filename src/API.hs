module API
  ( API
  , api
  )
where

import API.Health (HealthAPI, healthAPI)
import API.User (UserAPI, userAPI)
import qualified Capabilities.Crypto as C
import qualified Capabilities.Database as D
import RIO
import Servant ((:<|>) (..), ServerT)

type API
  = UserAPI
    :<|> HealthAPI

api :: (C.Crypto m, MonadThrow m, D.Database m) => ServerT API m
api = userAPI :<|> healthAPI
