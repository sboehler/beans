module API.Users
  ( UsersAPI,
    usersAPI,
  )
where

import qualified Capabilities.Crypto as CD
import qualified Capabilities.Persistence as CD
import Data.Aeson (FromJSON)
import qualified Database.Schema as D
import RIO
import Servant
  ( (:<|>) (..),
    (:>),
    Capture,
    Get,
    JSON,
    Post,
    ReqBody,
    ServerT,
    err404,
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
data Credentials
  = Credentials
      { credentialsEmail :: D.Email,
        credentialsPassword :: Text
      }
  deriving (Show, Eq, Generic, FromJSON)

type CreateUserR
  = "users"
      :> ReqBody '[JSON] Credentials
      :> Post '[JSON] D.User

createUser :: (CD.ManageUsers m, CD.Crypto m, MonadThrow m) => ServerT CreateUserR m
createUser credentials = do
  hashedPassword <- CD.hashPassword (encodeUtf8 $ credentialsPassword credentials)
  CD.createUser (credentialsEmail credentials) hashedPassword

--------------------------------------------------------------------------------
type UsersAPI = GetUsersR :<|> GetUserR :<|> CreateUserR

usersAPI :: (CD.ManageUsers m, CD.Crypto m, MonadThrow m) => ServerT UsersAPI m
usersAPI = getUsers :<|> getUser :<|> createUser
