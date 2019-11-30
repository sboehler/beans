module App
  ( startApp,
  )
where

import API (API, api)
import Control.Monad.Trans.Except (ExceptT (..))
import qualified Database as Db
import Env (Env (Env))
import Network.Wai(Request)
import Network.HTTP.Types.Status(Status)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setHost, setPort)
import RIO hiding (Handler)
import Servant
  ( Handler (Handler),
    Proxy (Proxy),
    hoistServer,
    serve,
  )

startApp :: FilePath -> IO ()
startApp dbfile = do
  Db.initialize dbfile
  logOptions <- logOptionsHandle stderr True
  pool <- Db.createPool dbfile
  let server =
        hoistServer
          (Proxy :: Proxy API)
          (transform pool logOptions)
          api
  let app = serve (Proxy :: Proxy API) server
  withLogFunc logOptions $ \lf -> do
    let settings = setPort 4004 $ setHost "*" $ setLogger (logger lf) $ defaultSettings
    runSettings settings app

logger :: LogFunc -> Request -> Status -> Maybe Integer -> IO()
logger lf r _s _i = runRIO lf $ do
  logInfo $ displayShow r


transform ::
  forall a.
  Db.ConnectionPool ->
  LogOptions ->
  RIO Env a ->
  Handler a
transform pool logOptions m =
  Handler
    $ ExceptT
    $ try
    $ withLogFunc logOptions
    $ \logFunc -> Db.withResource pool $ \con -> runRIO (Env con logFunc) m
