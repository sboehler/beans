module API.Users
  ( UsersAPI
  , usersAPI
  )
where

import qualified Capabilities.Database as CD
import Database.Beam
import qualified Database.Schema as D
import Lens.Micro.Platform ((^.))
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
getUsers = CD.runSelectMany (select (all_ (D.beansDb ^. D.dbUsers)))

--------------------------------------------------------------------------------
type UsersAPI = GetUsersR

usersAPI :: (CD.Database m) => ServerT UsersAPI m
usersAPI = getUsers
