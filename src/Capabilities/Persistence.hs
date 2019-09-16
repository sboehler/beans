module Capabilities.Persistence where

import Capabilities.Database as CD
import Database.Beam
import qualified Database.Schema as S
import Env
import RIO

--------------------------------------------------------------------------------
class (Monad m) => ManageUsers m where

  getUserById :: S.UserId -> m (Maybe S.User)

  getUsers :: m [S.User]

instance ManageUsers (RIO Env) where

  getUserById (S.UserId i) =
    CD.runSelectMaybe $ select $
      filter_ (\u -> (u ^. S.userId) ==. val_ i) $
      all_ (S.beansDb ^. S.dbUsers)

  getUsers = CD.runSelectMany (select (all_ (S.beansDb ^. S.dbUsers)))
