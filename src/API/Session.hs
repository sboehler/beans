module API.Session
  ( SessionAPI,
    sessionAPI,
  )
where

import qualified Capabilities.Crypto as C
import qualified Capabilities.Persistence as C
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding as T
import RIO
import qualified Schema as D
import Servant ((:>), JSON, Post, ReqBody, ServerT, err401)

data Credentials
  = Credentials
      { credentialsEmail :: D.Email,
        credentialsPassword :: Text
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

createSession :: (MonadThrow m, C.Crypto m, C.ManageUsers m, C.ManageSession m) => ServerT PostSessionR m
createSession creds = do
  user <- C.getUserByEmail (credentialsEmail creds) >>= maybe (throwM err401) return
  valid <- C.validatePassword (T.encodeUtf8 $ credentialsPassword creds) (user ^. D.hashedPassword)
  when (not valid) $ throwM err401
  token <- C.createSession user >>= either (const $ throwM err401) return
  return $ Token $ T.decodeUtf8 (toStrict token)

type SessionAPI = PostSessionR

sessionAPI :: (MonadThrow m, C.Crypto m, C.ManageUsers m, C.ManageSession m) => ServerT SessionAPI m
sessionAPI = createSession
