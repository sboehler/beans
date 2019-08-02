module Database.Schema where

import Data.Aeson (ToJSON)
import Data.Time.LocalTime (LocalTime)
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Migrate
import Database.Beam.Postgres (Postgres)
import RIO

data UserT f
  = User
      { _userId :: Columnar f Int64
      , _userEmail :: Columnar f Text
      , _userHashedPassword :: Columnar f Text
      , _userCreatedAt :: Columnar f LocalTime
      }
  deriving Generic

type User = UserT Identity

type UserId = PrimaryKey UserT Identity

deriving instance Show User

deriving instance Eq User

deriving instance ToJSON User

instance Beamable UserT

instance Table UserT where

  data PrimaryKey UserT f = UserId (Columnar f Int64)
    deriving (Generic, Beamable)

  primaryKey = UserId . _userId

--------------------------------------------------------------------------------
data BeansDb f
  = BeansDb
      {_beansUsers :: f (TableEntity UserT)}
  deriving (Generic, Database be)

beansDb :: DatabaseSettings be BeansDb
beansDb = defaultDbSettings

beansCheckedDb :: CheckedDatabaseSettings Postgres BeansDb
beansCheckedDb = defaultMigratableDbSettings @Postgres

-- TODO: move somewhere else
getUsers :: (MonadBeam be m, HasQBuilder be, BeamSqlBackend be, FromBackendRow be User) => m [User]
getUsers = do
  let allUsers = all_ (_beansUsers beansDb)
  runSelectReturningList $ select allUsers
