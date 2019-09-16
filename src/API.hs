module API
  ( API
  , api
  )
where

import API.Health (HealthAPI, healthAPI)
import API.Session (SessionAPI, sessionAPI)
import API.Users (UsersAPI, usersAPI)
import qualified Capabilities.Crypto as C
import qualified Capabilities.Persistence as C
import RIO
import Servant
  ( (:<|>) ((:<|>))
  , (:>)
  , ServerT
  )

type API
  = "api" :> (HealthAPI :<|> UsersAPI :<|> SessionAPI)

api :: (C.Crypto m, MonadThrow m, C.ManageUsers m, C.ManageSession m) => ServerT API m
api = healthAPI :<|> usersAPI :<|> sessionAPI
