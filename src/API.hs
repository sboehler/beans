module API
  ( API
  , api
  ) where

import API.Session (SessionAPI, sessionAPI)
import API.User (UserAPI, userAPI)
import qualified Capabilities.Database as D
import Capabilities.Error
import qualified Capabilities.Session as S
import Control.Monad.Freer
import Servant ((:<|>)(..), Handler, ServerT)

type API
   = SessionAPI
     :<|> UserAPI

api ::
     ( LastMember Handler effs
     , Members '[ S.Session, D.Database, AppError, Handler] effs
     )
  => ServerT API (Eff effs)
api = sessionAPI :<|> userAPI
