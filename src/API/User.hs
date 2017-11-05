module API.User
  ( UserAPI
  , userAPI
  ) where

import qualified Capabilities.Database as D
import Capabilities.Error
import Control.Lens
import Control.Monad.Freer
import qualified Database.Users as Users
import qualified Model as M
import Servant
  ( (:<|>)((:<|>))
  , (:>)
  , Get
  , Handler
  , JSON
  , Post
  , ReqBody
  , ServerT
  )
import Servant.Auth.Server (Auth, AuthResult(Authenticated), Cookie)

--------------------------------------------------------------------------------
type GetUserR
   = Auth '[ Cookie] M.UserSession
     :> "user"
     :> Get '[ JSON] (M.Entity M.User)

getUser ::
     (Members '[ D.Database, AppError] effs)
  => AuthResult M.UserSession
  -> Eff effs (M.Entity M.User)
getUser (Authenticated (M.UserSession userId)) = do
  user <- Users.get userId
  maybe (throwError NotFound) return user
getUser _ = throwError Unauthorized

--------------------------------------------------------------------------------
type PostUserR
   = "user"
     :> ReqBody '[ JSON] M.Credentials
     :> Post '[ JSON] (M.Entity M.User)

createUser ::
     (LastMember Handler effs, Members '[ D.Database, AppError, Handler] effs)
  => M.Credentials
  -> Eff effs (M.Entity M.User)
createUser credentials = do
  existingUser <- Users.getByEmail (credentials ^. M.email)
  case existingUser of
    Just _ -> throwError BadRequest
    Nothing ->
      M.createUser credentials >>= Users.insert >>=
      maybe (throwError InternalError) return

--------------------------------------------------------------------------------
type UserAPI
   = GetUserR
     :<|> PostUserR

userAPI ::
     (LastMember Handler effs, Members '[ D.Database, AppError, Handler] effs)
  => ServerT UserAPI (Eff effs)
userAPI = getUser :<|> createUser
