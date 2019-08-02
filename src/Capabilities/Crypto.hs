module Capabilities.Crypto
  ( Crypto (..)
  )
where

import qualified Crypto.KDF.BCrypt as C
import qualified Data.ByteString.Char8 as B
import RIO

newtype HashedPassword = HashedPassword B.ByteString

class Crypto m where

  validatePassword :: String -> HashedPassword -> m Bool

  hashPassword :: String -> m HashedPassword

--------------------------------------------------------------------------------
instance Crypto (RIO a) where

  validatePassword p (HashedPassword h) = return $ C.validatePassword (B.pack p) h

  hashPassword pwd = do
    hash <- liftIO $ C.hashPassword 12 (B.pack pwd)
    return $ HashedPassword hash
