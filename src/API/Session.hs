module API.Session
  ( SessionAPI
  , sessionAPI
  )
where

import qualified Capabilities.Database as CD
import Data.Aeson (ToJSON)
import qualified Database.Schema as D
import RIO
import Servant ((:>), JSON, Post, ReqBody, ServerT)

data Credentials
  = Credentials
      { credentialsEmail :: D.Email
      , credentialsPassword :: Text
      }
  deriving (Show, Eq, Generic)

newtype Token
  = Token
      { token :: Text
      }
  deriving (Generic, Show, Eq, ToJSON)

type PostSessionR
  = "session"
    :> ReqBody '[JSON] Credentials
      :> Post '[JSON] Token

createSession :: (CD.Database m) => ServerT PostSessionR m
createSession _creds = return $ Token ""

type SessionAPI = PostSessionR

sessionAPI :: (CD.Database m) => ServerT SessionAPI m
sessionAPI = createSession
