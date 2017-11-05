module App
  ( startApp
  ) where

import API (API, api)
import qualified Capabilities.Database as D
import Capabilities.Error hiding (throwError)
import qualified Capabilities.Session as S
import Control.Monad.Catch (catchIOError)
import Control.Monad.Freer hiding (run)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Pool as P
import qualified Database.PostgreSQL.Simple as PG
import Network.Wai.Handler.Warp (run)
import Servant
  ( Context((:.), EmptyContext)
  , Handler
  , Proxy(Proxy)
  , err400
  , err401
  , err404
  , err500
  , hoistServerWithContext
  , serveWithContext
  , throwError
  )
import Servant.Auth.Server
  ( CookieSettings
  , IsSecure(NotSecure)
  , JWTSettings
  , cookieIsSecure
  , def
  , defaultJWTSettings
  , generateKey
  )

startApp :: IO ()
startApp
  -- create a key to sign cookies
 = do
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
          (Proxy :: Proxy '[ CookieSettings, JWTSettings])
          (transform cookieSettings jwtSettings pool)
          api
  -- create the appliation
  let app = serveWithContext (Proxy :: Proxy API) context server
  -- run the application
  run port app

transform ::
     forall a.
     CookieSettings
  -> JWTSettings
  -> P.Pool D.Connection
  -> Eff '[ S.Session, D.Database, AppError, Handler] a
  -> Handler a
transform cookieSettings jwtSettings pool eff = do
  res <- P.withResource pool runEffects
  either mapError return res
  where
    runEffects :: D.Connection -> Handler (Either Err a)
    runEffects con =
      withTransaction
        con
        (runM .
         runError . D.runDatabase con . S.runSession cookieSettings jwtSettings $
         eff)

withTransaction ::
     D.Connection -> Handler (Either Err a) -> Handler (Either Err a)
withTransaction con handler = do
  liftIO $ PG.begin con
  res <- handler `catchIOError` (\_ -> return $ Left InternalError)
  liftIO $
    case res of
      Left _ -> PG.rollback con
      Right _ -> PG.commit con
  return res

mapError :: Err -> Handler a
mapError =
  throwError . \case
    BadRequest -> err400
    Unauthorized -> err401
    NotFound -> err404
    InternalError -> err500
