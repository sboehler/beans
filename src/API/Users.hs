module API.Users
  ( UsersAPI
  , usersAPI
  )
where

import qualified Capabilities.Database as CD
import Database.Beam
import qualified Database.Schema as D
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

getUsers :: (CD.Database m n) => ServerT GetUsersR m
getUsers = CD.runSelectMany (select (all_ (D._beansUsers D.beansDb)))

--------------------------------------------------------------------------------
type UsersAPI = GetUsersR

usersAPI :: (CD.Database m n) => ServerT UsersAPI m
usersAPI = getUsers
