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
  ( (:<|>) ((:<|>))
  , (:>)
  , Get
  , JSON
  , NewlineFraming
  , ServerT
  , SourceIO
  , StreamGet
  )
import Servant.Conduit

--------------------------------------------------------------------------------
type GetUsersR
  = "users"
    :> Get '[JSON] [D.User]

getUsers :: (CD.Database m) => ServerT GetUsersR m
getUsers = do
  CD.runStatement $ D.getUsers

--------------------------------------------------------------------------------
type GetUsersR2
  = "users2"
    :> Get '[JSON] [D.User]

getUsers2 :: (MonadIO m, CD.Database m) => ServerT GetUsersR2 m
getUsers2 = do
  con <- CD.getConnection
  let allUsers = select (all_ (D._beansUsers D.beansDb))
  u <- runConduit $ liftIO $ runSelect con allUsers (\c -> runConduit (c .| consume))
  return u

type GetUsersR3
  = "users3"
    :> StreamGet NewlineFraming JSON (SourceIO D.User)

getUsers3 :: (MonadIO m, CD.Database m) => ServerT GetUsersR3 m
getUsers3 = do
  con <- CD.getConnection
  let allUsers = select (all_ (D._beansUsers D.beansDb))
  liftIO $ runSelect con allUsers (return . conduitToSourceIO)

--------------------------------------------------------------------------------
type UsersAPI = GetUsersR :<|> GetUsersR2 :<|> GetUsersR3

usersAPI :: (CD.Database m) => ServerT UsersAPI m
usersAPI = getUsers :<|> getUsers2 :<|> getUsers3
