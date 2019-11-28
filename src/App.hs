module App
  ( startApp,
  )
where

import API (API, api)
import Control.Monad.Trans.Except (ExceptT (..))
import Database (ConnectionPool, createPool, withResource)
import Env (Env (Env))
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import RIO hiding (Handler)
import Servant
  ( Handler (Handler),
    Proxy (Proxy),
    hoistServer,
    serve,
  )

startApp :: FilePath -> IO ()
startApp dbfile = do
  pool <- createPool dbfile
  let server =
        hoistServer
          (Proxy :: Proxy API)
          (transform pool)
          api
  let app = serve (Proxy :: Proxy API) server
  runSettings (setPort 4004 $ setHost "*" $ defaultSettings) app

transform ::
  forall a.
  ConnectionPool ->
  RIO Env a ->
  Handler a
transform pool m =
  Handler
    $ ExceptT
    $ try
    $ withResource pool
    $ \con -> runRIO (Env con) m
