module API.Session where

import qualified Capabilities.Session as S
import Control.Monad.Freer
import qualified Model as M
import Servant
  ( (:<|>)(..)
  , (:>)
  , DeleteNoContent
  , Header
  , Headers
  , JSON
  , NoContent(..)
  , PostNoContent
  , ReqBody
  , ServerT
  )
import Servant.Auth.Server (SetCookie)

type PostSessionR
   = "session"
     :> ReqBody '[ JSON] M.Credentials
     :> PostNoContent '[ JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

type DeleteSessionR
   = "session"
     :> DeleteNoContent '[ JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

type SessionAPI
   = PostSessionR
     :<|> DeleteSessionR

sessionAPI :: Member S.Session effs => ServerT SessionAPI (Eff effs)
sessionAPI = S.login :<|> S.logout
