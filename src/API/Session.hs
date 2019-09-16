module API.Session
  ( SessionAPI
  , sessionAPI
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Database.Schema as D
import RIO
import Servant ((:>), JSON, Post, ReqBody, ServerT)

data Credentials
  = Credentials
      { credentialsEmail :: D.Email
      , credentialsPassword :: Text
      }
  deriving (Show, Eq, Generic, FromJSON)

newtype Token
  = Token
      { token :: Text
      }
  deriving (Generic, Show, Eq, ToJSON)

type PostSessionR
  = "session"
    :> ReqBody '[JSON] Credentials
      :> Post '[JSON] Token

createSession :: Monad m => ServerT PostSessionR m
createSession _creds = return $ Token ""

type SessionAPI = PostSessionR

sessionAPI :: (Monad m) => ServerT SessionAPI m
sessionAPI = createSession
