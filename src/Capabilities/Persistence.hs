module Capabilities.Persistence where

import Capabilities.Database as CD
import qualified Crypto.JOSE.Error as E
import Data.ByteString.Lazy as B
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Database.Beam
import Env
import RIO
import qualified Schema as S
import Servant.Auth.Server (makeJWT)

--------------------------------------------------------------------------------
class (Monad m) => ManageUsers m where

  getUserById :: S.UserId -> m (Maybe S.User)

  getUserByEmail :: S.Email -> m (Maybe S.User)

  getUsers :: m [S.User]

  createUser :: S.Email -> S.HashedPassword -> m S.User

instance ManageUsers (RIO Env) where

  getUserById (S.UserId i) =
    CD.runSelectMaybe $ select
      $ filter_ (\u -> (u ^. S.userId) ==. val_ i)
      $ all_ (S.beansDb ^. S.dbUsers)

  getUserByEmail (email) =
    CD.runSelectMaybe $ select
      $ filter_ (\u -> (u ^. S.email) ==. val_ email)
      $ all_ (S.beansDb ^. S.dbUsers)

  getUsers = CD.runSelectMany (select (all_ (S.beansDb ^. S.dbUsers)))

  createUser email password = do
    users <-
      CD.runInsertReturningList
        $ insert (S.beansDb ^. S.dbUsers)
        $ insertExpressions [S.User default_ (val_ email) (val_ password) default_]
    case users of
      [user] -> return user
      _ -> error "inconsistency"

--------------------------------------------------------------------------------
class (Monad m) => ManageSession m where
  createSession :: S.User -> m (Either E.Error B.ByteString)

instance ManageSession (RIO Env) where
  createSession user = do
    js <- view jwtSettings
    expiry <- addUTCTime (12 * 3600) <$> (liftIO getCurrentTime)
    liftIO $ makeJWT user js (Just expiry)
