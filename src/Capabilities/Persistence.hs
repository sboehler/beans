module Capabilities.Persistence where

import Capabilities.Database as CD
import qualified Crypto.JOSE.Error as E
import Data.ByteString.Lazy as B
import Database.Beam
import qualified Database.Schema as S
import Env
import RIO
import Servant.Auth.Server (makeJWT)

--------------------------------------------------------------------------------
class (Monad m) => ManageUsers m where

  getUserById :: S.UserId -> m (Maybe S.User)

  getUserByEmail :: S.Email -> m (Maybe S.User)

  getUsers :: m [S.User]

instance ManageUsers (RIO Env) where

  getUserById (S.UserId i) =
    CD.runSelectMaybe $ select $
      filter_ (\u -> (u ^. S.userId) ==. val_ i) $
      all_ (S.beansDb ^. S.dbUsers)

  getUserByEmail (email) =
    CD.runSelectMaybe $ select $
      filter_ (\u -> (u ^. S.email) ==. val_ email) $
      all_ (S.beansDb ^. S.dbUsers)

  getUsers = CD.runSelectMany (select (all_ (S.beansDb ^. S.dbUsers)))

--------------------------------------------------------------------------------
class (Monad m) => ManageSession m where

  createSession :: S.User -> m (Either E.Error B.ByteString)

instance ManageSession (RIO Env) where

  createSession user = do
    js <- view jwtSettings
    liftIO $ makeJWT user js Nothing
