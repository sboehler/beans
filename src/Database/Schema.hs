module Database.Schema where

import Data.Time.Clock (UTCTime)
import Database.Beam
import RIO

data UserT f
  = User
      { _userId :: Columnar f Integer
      , _userEmail :: Columnar f Text
      , _userHashedPassword :: Columnar f Text
      , _userCreatedAt :: Columnar f UTCTime
      }
  deriving Generic
