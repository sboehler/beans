module API.User
  ( UserAPI
  , userAPI
  ) where

import qualified Database.Users as Users
import Env
import qualified Model as M
import RIO
import Servant
  ( (:<|>)((:<|>))
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
import Servant.Auth.Server (Auth, AuthResult(Authenticated), Cookie)

--------------------------------------------------------------------------------
type GetUserR
   = Auth '[ Cookie] M.UserSession
     :> "user"
     :> Get '[ JSON] (M.Entity M.User)

getUser :: AuthResult M.UserSession -> RIO Env (M.Entity M.User)
getUser (Authenticated (M.UserSession userId)) = do
  user <- Users.get userId
  maybe (throwIO err404) return user
getUser _ = throwIO err401

--------------------------------------------------------------------------------
type PostUserR
   = "user"
     :> ReqBody '[ JSON] M.Credentials
     :> Post '[ JSON] (M.Entity M.User)

createUser :: M.Credentials -> RIO Env (M.Entity M.User)
createUser credentials = do
  existingUser <- Users.getByEmail (credentials ^. M.email)
  case existingUser of
    Just _ -> throwIO err400
    Nothing ->
      M.createUser credentials >>= Users.insert >>=
      maybe (throwIO err500) return

--------------------------------------------------------------------------------
type UserAPI
   = GetUserR
     :<|> PostUserR

userAPI :: ServerT UserAPI (RIO Env)
userAPI = getUser :<|> createUser
