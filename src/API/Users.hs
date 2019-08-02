module API.Users
  ( UsersAPI
  , usersAPI
  )
where

import qualified Capabilities.Database as CD
import qualified Database.Schema as D
import RIO
import Servant
  ( (:>)
  , Get
  , JSON
  , ServerT
  )

--------------------------------------------------------------------------------
type GetUsersR
  = "users"
    :> Get '[JSON] [D.User]

getUsers :: (CD.Database m) => ServerT GetUsersR m
getUsers = do
  CD.runStatement $ D.getUsers

--------------------------------------------------------------------------------
type UsersAPI = GetUsersR

usersAPI :: (CD.Database m) => ServerT UsersAPI m
usersAPI = getUsers
