module App
  ( startApp,
  )
where

import API (API, api)
import qualified Config as C
import Control.Monad.Trans.Except (ExceptT (..))
import Crypto.JOSE.JWK ()
import Env (Env (Env))
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import RIO hiding (Handler)
import Servant
  ( Handler (Handler),
    Proxy (Proxy),
    hoistServer,
    serve,
  )

startApp :: IO ()
startApp = do
  -- read configuration
  config <- C.getConfig
  -- create a pool of database connections
  let server =
        hoistServer
          (Proxy :: Proxy API)
          (transform config)
          api
  -- create the appliation
  let app = serve (Proxy :: Proxy API) server
  let port = fromIntegral $ config ^. C.appPort
  -- run the application
  runSettings (setPort port $ setHost "*" $ defaultSettings) app

transform ::
  forall a.
  C.Config ->
  RIO Env a ->
  Handler a
transform config m =
  Handler
    $ ExceptT
    $ try
    $ do
      let env = Env config
      runRIO env m
