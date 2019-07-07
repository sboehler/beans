{-# LANGUAGE UndecidableInstances #-}

module Model
  ( Entity
  , id
  , model
  , createdAt
  , User
  , createUser
  , email
  , password
  , HashedPassword
  , hashPassword
  , validatePassword
  , Email
  , UserId
  , Credentials
  , UserSession(..)
  ) where

import qualified Crypto.KDF.BCrypt as CKB
import Data.Aeson (FromJSON, ToJSON, Value(..), (.=), object, toJSON)
import qualified Data.ByteString.Char8 as B
import Data.Serialize (Serialize)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import GHC.Generics (Generic)
import Lens.Micro.Platform (makeFields)
import RIO hiding (id)
import qualified RIO.HashMap as H
import qualified RIO.Time as T
import Servant.Auth.Server (FromJWT, ToJWT)

--------------------------------------------------------------------------------
-- A type family for database ids
--------------------------------------------------------------------------------
type family Id a

-- A wrapper for models
data Entity model =
  Entity
    { _entityId :: Id model
    , _entityModel :: model
    , _entityCreatedAt :: T.UTCTime
    }

makeFields ''Entity

deriving instance
         (Show (Id model), Show model) => Show (Entity model)

instance (ToField (Id model), ToRow model) => ToRow (Entity model) where
  toRow (Entity i m _t) = toField i : toRow m

instance (FromField (Id model), FromRow model) => FromRow (Entity model) where
  fromRow = Entity <$> field <*> fromRow <*> field

instance (ToJSON model, ToJSON (Id model)) => ToJSON (Entity model) where
  toJSON e =
    case toJSON (e ^. model) of
      Object m -> Object $ m `H.union` l
      j -> Object $ H.singleton "value" j `H.union` l
    where
      l =
        H.fromList
          [("id", toJSON (e ^. id)), ("createdAt", toJSON (e ^. createdAt))]

--------------------------------------------------------------------------------
newtype UserId =
  UserId Int
  deriving (Eq, Show, Read, Generic, FromField, ToField, FromJSON, ToJSON)

instance Serialize UserId

--------------------------------------------------------------------------------
newtype Email =
  Email String
  deriving (Show, Eq, Read, FromField, ToField, Generic, FromJSON, ToJSON)

--------------------------------------------------------------------------------
newtype HashedPassword =
  HashedPassword B.ByteString
  deriving (Show, Eq, Read, FromField, ToField)

validatePassword :: String -> HashedPassword -> Bool
validatePassword p (HashedPassword h) = CKB.validatePassword (B.pack p) h

hashPassword :: String -> RIO e HashedPassword
hashPassword password = do
  hash <- liftIO $ CKB.hashPassword 12 (B.pack password)
  return $ HashedPassword hash

--------------------------------------------------------------------------------
data User =
  User
    { _userEmail :: Email
    , _userPassword :: HashedPassword
    }
  deriving (Show, Eq, Generic)

makeFields ''User

createUser :: Credentials -> RIO e User
createUser credentials = do
  hashedPassword <- hashPassword (credentials ^. password)
  return $ User (credentials ^. email) hashedPassword

instance ToJSON User where
  toJSON u = object ["email" .= (u ^. email)]

instance FromRow User

type instance Id User = UserId

--------------------------------------------------------------------------------
data Credentials =
  Credentials
    { _credentialsEmail :: Email
    , _credentialsPassword :: String
    }
  deriving (Show, Eq, Generic)

makeFields ''Credentials

instance FromJSON Credentials

--------------------------------------------------------------------------------
newtype UserSession =
  UserSession UserId
  deriving (Eq, Show, Read, Generic)

instance FromJSON UserSession

instance ToJSON UserSession

instance FromJWT UserSession

instance ToJWT UserSession
