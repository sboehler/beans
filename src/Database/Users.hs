module Database.Users
  ( getByEmail
  , get
  , insert
  )
where

import qualified Capabilities.Database as D
import Lens.Micro.Platform ((^.))
import qualified Model as M
import RIO hiding (id)

get :: D.Database m => M.UserId -> m (Maybe (M.Entity M.User))
get id =
  D.fetch1
    "select id, email, hashed_password, created_at from users where id = ?"
    [id]

getByEmail :: D.Database m => M.Email -> m (Maybe (M.User))
getByEmail email = D.fetch1 "select * from users where email = ?" [email]

insert :: D.Database m => M.User -> m (Maybe (M.Entity M.User))
insert u =
  D.fetch1
    "insert into users (email, hashed_password) values (?, ?) returning *"
    (u ^. M.email, u ^. M.password)
