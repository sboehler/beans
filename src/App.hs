module App
  ( startApp,
  )
where

import API (API, api)
import Capabilities.Crypto ()
import qualified Capabilities.Database as D
import qualified Config as C
import Control.Monad.Trans.Except (ExceptT (..))
import Crypto.JOSE.JWK ()
import Data.Aeson (decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromJust)
import qualified Data.Pool as P
import qualified Data.Text.Encoding as E
import qualified Database.PostgreSQL.Simple as PG
import Env (Env (Env))
import Network.Wai.Handler.Warp (run)
import RIO hiding (Handler)
import Servant
  ( Context ((:.), EmptyContext),
    Handler (Handler),
    HasServer,
    Proxy (Proxy),
    ServerT,
    hoistServerWithContext,
    serveWithContext,
  )
import Servant.Auth.Server
  ( CookieSettings,
    IsSecure (NotSecure),
    JWTSettings,
    cookieIsSecure,
    def,
    defaultJWTSettings,
  )

startApp :: IO ()
startApp = do
  -- read configuration
  config <- C.getConfig
  -- load the key to sign cookies
  let myKey = fromJust . decode . fromStrict . E.encodeUtf8 $ (config ^. C.appKey)
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
        hoistServerWithAuth
          (Proxy :: Proxy API)
          (transform config cookieSettings jwtSettings pool)
          api
  -- create the appliation
  let app = serveWithContext (Proxy :: Proxy API) context server
  let port = fromIntegral $ config ^. C.appPort
  -- run the application
  run port app

hoistServerWithAuth
  :: HasServer api '[CookieSettings, JWTSettings]
  => Proxy api
  -> (forall x. m x -> n x)
  -> ServerT api m
  -> ServerT api n
hoistServerWithAuth a =
  hoistServerWithContext a (Proxy :: Proxy '[CookieSettings, JWTSettings])

transform
  :: forall a.
     C.Config
  -> CookieSettings
  -> JWTSettings
  -> P.Pool D.Connection
  -> RIO Env a
  -> Handler a
transform config cookieSettings jwtSettings pool m =
  Handler
    $ ExceptT
    $ try
    $ P.withResource pool
    $ \con ->
      PG.withTransaction con $ do
        let env = Env config cookieSettings jwtSettings con
        runRIO env m
