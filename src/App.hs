module App
  ( startApp
  )
where

import API (API, api)
import qualified Capabilities.Database as D
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
  -- create a key to sign cookies
  myKey <- generateKey
  -- create a pool of database connections
  pool <- D.createPool
  -- apply database migrations
  P.withResource pool (D.initializeDatabase "config/schema")
  -- cookie settings
  let cookieSettings = def {cookieIsSecure = NotSecure}
  -- jwt settings
  let jwtSettings = defaultJWTSettings myKey
  -- the port on which we are running the server
  let port = 4000
  -- servant context
  let context =
        def {cookieIsSecure = NotSecure} :. defaultJWTSettings myKey :.
          EmptyContext
  -- create a server
  let server =
        hoistServerWithContext
          (Proxy :: Proxy API)
          (Proxy :: Proxy '[CookieSettings, JWTSettings])
          (transform cookieSettings jwtSettings pool)
          api
  -- create the appliation
  let app = serveWithContext (Proxy :: Proxy API) context server
  -- run the application
  run port app

transform
  :: forall a. CookieSettings
  -> JWTSettings
  -> P.Pool D.Connection
  -> RIO Env a
  -> Handler a
transform cookieSettings jwtSettings pool m =
  Handler $
    ExceptT $
    try $
    P.withResource pool $ \con ->
    PG.withTransaction con $ do
      let env = Env con cookieSettings jwtSettings
      runRIO env m
