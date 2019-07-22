module Capabilities.Crypto
  ( Crypto (..)
  )
where

import qualified Crypto.KDF.BCrypt as C
import qualified Data.ByteString.Char8 as B
import qualified Model as M
import RIO

class Crypto m where

  validatePassword :: String -> M.HashedPassword -> m Bool

  hashPassword :: String -> m M.HashedPassword

--------------------------------------------------------------------------------
instance Crypto (RIO a) where

  validatePassword p (M.HashedPassword h) = return $ C.validatePassword (B.pack p) h

  hashPassword pwd = do
    hash <- liftIO $ C.hashPassword 12 (B.pack pwd)
    return $ M.HashedPassword hash
