module API.Users
  ( UsersAPI
  , usersAPI
  )
where

import qualified Capabilities.Database as CD
import Data.Conduit
import Data.Conduit.List
import Database.Beam
import Database.Beam.Postgres.Conduit
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

getUsers :: (MonadIO m, CD.Database m) => ServerT GetUsersR m
getUsers = do
  con <- CD.getConnection
  let allUsers = select (all_ (D._beansUsers D.beansDb))
  runConduit $ liftIO $ runSelect con allUsers (\c -> runConduit (c .| consume))

--------------------------------------------------------------------------------
type UsersAPI = GetUsersR

usersAPI :: (CD.Database m) => ServerT UsersAPI m
usersAPI = getUsers
