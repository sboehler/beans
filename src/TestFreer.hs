module TestFreer where

import qualified Network.Wai.Handler.Warp as Warp

import Servant hiding (Unauthorized)

import Capabilities.Database
import Capabilities.Error hiding (throwError)
import Control.Monad.Freer
import Control.Monad.IO.Class (liftIO)
import qualified Data.Pool as P
import qualified Model as M

type API
   = "a"
     :> Get '[ JSON] [M.Entity M.User]

endpoint :: Member Database effs => ServerT API (Eff effs)
endpoint = fetchIt

type Endpoint a
   = forall effs. (Member Database effs) =>
                    Eff effs a

fetchIt :: Endpoint [M.Entity M.User]
fetchIt = do
  a <- fetchN_ "select * from users"
  b <- fetchN_ "select * from users"
  return $ a ++ b

startApp :: IO ()
startApp = Warp.run 3000 app
  where
    app = serve (Proxy :: Proxy API) server
    server = hoistServer (Proxy :: Proxy API) convert endpoint

convert :: Eff '[ Database, AppError, Handler] a -> Handler a
convert eff = do
  pool <- liftIO createPool
  res <- P.withResource pool $ \con -> runM . runError . runDatabase con $ eff
  case res of
    Left NotFound -> throwError err404
    Left InternalError -> throwError err500
    Left Unauthorized -> throwError err401
    Left BadRequest -> throwError err400
    Right e -> pure e

main :: IO ()
main = startApp
