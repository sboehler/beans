module API.User
  ( UserAPI
  , userAPI
  )
where

import qualified Capabilities.Crypto as C
import qualified Capabilities.Database as D
import qualified Database.Users as Users
import qualified Model as M
import RIO
import Servant
  ( (:<|>) ((:<|>))
  , (:>)
  , Get
  , JSON
  , Post
  , ReqBody
  , ServerT
  , err400
  , err401
  , err404
  , err500
  )
import Servant.Auth.Server (Auth, AuthResult (Authenticated), Cookie)

--------------------------------------------------------------------------------
type GetUserR = Auth '[Cookie] M.UserSession :> "user" :> Get '[JSON] (M.Entity M.User)

getUser :: (MonadThrow m, D.Database m) => AuthResult M.UserSession -> m (M.Entity M.User)
getUser (Authenticated (M.UserSession userId)) = do
  user <- Users.get userId
  maybe (throwM err404) return user
getUser _ = throwM err401

--------------------------------------------------------------------------------
type PostUserR
  = "user"
    :> ReqBody '[JSON] M.Credentials
      :> Post '[JSON] (M.Entity M.User)

createUser :: (C.Crypto m, MonadThrow m, D.Database m) => M.Credentials -> m (M.Entity M.User)
createUser credentials = do
  existingUser <- Users.getByEmail (credentials ^. M.email)
  case existingUser of
    Just _ -> throwM err400
    Nothing -> do
      hashedPassword <- C.hashPassword (credentials ^. M.password)
      let user = M.User (credentials ^. M.email) hashedPassword
      Users.insert user >>= maybe (throwM err500) return

--------------------------------------------------------------------------------
type UserAPI
  = GetUserR
    :<|> PostUserR

userAPI :: (C.Crypto m, MonadThrow m, D.Database m) => ServerT UserAPI m
userAPI = getUser :<|> createUser
