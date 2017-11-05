module Database.Users
  ( getByEmail
  , get
  , insert
  ) where

import Prelude hiding (id)

import qualified Capabilities.Database as D
import Control.Lens ((^.))
import Control.Monad.Freer (Eff, Members)
import qualified Model as M

get ::
     Members '[ D.Database] effs
  => M.UserId
  -> Eff effs (Maybe (M.Entity M.User))
get id = D.fetch1 "select * from users where id = ?" [id]

getByEmail ::
     Members '[ D.Database] effs
  => M.Email
  -> Eff effs (Maybe (M.Entity M.User))
getByEmail email = D.fetch1 "select * from users where email = ?" [email]

insert ::
     Members '[ D.Database] effs => M.User -> Eff effs (Maybe (M.Entity M.User))
insert u =
  D.fetch1
    "insert into users (email, hashed_password) values (?, ?) returning *"
    (u ^. M.email, u ^. M.password)
