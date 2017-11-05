module Capabilities.Session where

import qualified Capabilities.Database as D
import Capabilities.Error
import Control.Lens ((^.))
import Control.Monad.Freer
import Control.Monad.Trans (liftIO)
import qualified Database.Users as D
import qualified Model as M
import Servant (Handler, Header, Headers, NoContent(..), noHeader)
import Servant.Auth.Server (CookieSettings, JWTSettings, SetCookie, acceptLogin)

data Session a where
  Login
    :: M.Credentials
    -> Session (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
  Logout
    :: Session (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

login ::
     (Member Session effs)
  => M.Credentials
  -> Eff effs (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
login = send . Login

logout ::
     (Member Session effs)
  => Eff effs (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
logout = send Logout

--------------------------------------------------------------------------------
-- Interpreter in Handler
--------------------------------------------------------------------------------
runSession ::
     forall effs a.
     (LastMember Handler effs, Members '[ AppError, D.Database, Handler] effs)
  => CookieSettings
  -> JWTSettings
  -> Eff (Session ': effs) a
  -> Eff effs a
runSession cookieSettings jwtSettings =
  interpret $ \case
    Login credentials -> do
      user <- D.getByEmail (credentials ^. M.email)
      case user of
        Just u
          | M.validatePassword
             (credentials ^. M.password)
             (u ^. (M.model . M.password)) ->
            loginUser cookieSettings jwtSettings (u ^. M.id)
        _ -> throwError Unauthorized
    Logout -> noHeader . noHeader <$> return NoContent

loginUser ::
     (LastMember Handler effs, Members '[ AppError, Handler] effs)
  => CookieSettings
  -> JWTSettings
  -> M.UserId
  -> Eff effs (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginUser cookieSettings jwtSettings userId = do
  mApplyCookies <-
    sendM $
    liftIO $ acceptLogin cookieSettings jwtSettings (M.UserSession userId)
  case mApplyCookies of
    Nothing -> throwError Unauthorized
    Just applyCookies -> return $ applyCookies NoContent
