module API
  ( API
  , api
  )
where

import API.Health (HealthAPI, healthAPI)
import API.Users (UsersAPI, usersAPI)
import qualified Capabilities.Crypto as C
import qualified Capabilities.Database as D
import RIO
import Servant
  ( (:<|>) ((:<|>))
  , (:>)
  , ServerT
  )

type API
  = "api" :> (HealthAPI :<|> UsersAPI)

api :: (C.Crypto m, MonadThrow m, D.Database m n) => ServerT API m
api = healthAPI :<|> usersAPI
