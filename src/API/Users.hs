module API.Users
  ( UsersAPI
  , usersAPI
  )
where

import qualified Capabilities.Persistence as CD
import qualified Database.Schema as D
import RIO
import Servant
  ( (:<|>) (..)
  , (:>)
  , Capture
  , Get
  , JSON
  , ServerT
  , err404
  )

--------------------------------------------------------------------------------
type GetUsersR
  = "users"
    :> Get '[JSON] [D.User]

getUsers :: (CD.ManageUsers m) => ServerT GetUsersR m
getUsers = CD.getUsers

--------------------------------------------------------------------------------
type GetUserR
  = "users"
    :> Capture "id" Int64
      :> Get '[JSON] D.User

getUser :: (CD.ManageUsers m, MonadThrow m) => ServerT GetUserR m
getUser userId =
  CD.getUserById (D.UserId userId) >>= maybe (throwM err404) return

--------------------------------------------------------------------------------
type UsersAPI = GetUsersR :<|> GetUserR

usersAPI :: (CD.ManageUsers m, MonadThrow m) => ServerT UsersAPI m
usersAPI = getUsers :<|> getUser
