module Capabilities.Crypto
  ( Crypto (..),
  )
where

import qualified Crypto.KDF.BCrypt as C
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T
import qualified Database.Schema as D
import RIO

class Monad m => Crypto m where

  validatePassword :: B.ByteString -> D.HashedPassword -> m Bool

  hashPassword :: B.ByteString -> m D.HashedPassword

--------------------------------------------------------------------------------
instance Crypto (RIO a) where

  validatePassword p (D.HashedPassword h) = return $ C.validatePassword p (T.encodeUtf8 h)

  hashPassword pwd =
    liftIO $ D.HashedPassword . T.decodeUtf8 <$> C.hashPassword 12 pwd
