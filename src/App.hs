module App
  ( startApp
  )
where

import API (API, api)
import Capabilities.Crypto ()
import qualified Capabilities.Database as D
import qualified Config as C
import Control.Monad.Trans.Except
import qualified Data.Pool as P
import qualified Database.PostgreSQL.Simple as PG
import Env (Env (Env))
import Network.Wai.Handler.Warp (run)
import RIO hiding (Handler)
import Servant
  ( Context ((:.), EmptyContext)
  , Handler (Handler)
  , Proxy (Proxy)
  , hoistServerWithContext
  , serveWithContext
  )
import Servant.Auth.Server
  ( CookieSettings
  , IsSecure (NotSecure)
  , JWTSettings
  , cookieIsSecure
  , def
  , defaultJWTSettings
  , generateKey
  )

startApp :: IO ()
startApp = do
  -- read configuration
  config <- C.getConfig
  -- create a key to sign cookies
  myKey <- generateKey
  -- create a pool of database connections
  pool <- D.createPool config
  -- apply database migrations
  P.withResource pool (D.initializeDatabase "config/schema")
  -- cookie settings
  let cookieSettings = def {cookieIsSecure = NotSecure}
  -- jwt settings
  let jwtSettings = defaultJWTSettings myKey
  -- servant context
  let context = cookieSettings :. jwtSettings :. EmptyContext
  -- create a server
  let server =
        hoistServerWithContext
          (Proxy :: Proxy API)
          (Proxy :: Proxy '[CookieSettings, JWTSettings])
          (transform config cookieSettings jwtSettings pool)
          api
  -- create the appliation
  let app = serveWithContext (Proxy :: Proxy API) context server
  let port = fromIntegral $ config ^. C.appPort
  -- run the application
  run port app

transform
  :: forall a. C.Config
  -> CookieSettings
  -> JWTSettings
  -> P.Pool D.Connection
  -> RIO Env a
  -> Handler a
transform config cookieSettings jwtSettings pool m =
  Handler $
    ExceptT $
    try $
    P.withResource pool $ \con ->
    PG.withTransaction con $ do
      let env = Env config cookieSettings jwtSettings con
      runRIO env m
