module API.Protected where

import           Data.ByteString
import           Data.Text           (Text)
import           Relude
import           Servant             hiding (BasicAuth)
import qualified Servant             as S
import           Servant.Auth.Server
import           Servant.Client
import           AuthToken 
import           User

type BasicAuthProtectedAPI = "login" :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthToken)

-- Used for client
basicAuthProtectedAPI :: Proxy (S.BasicAuth "enter point" AuthToken :> BasicAuthProtectedAPI)
basicAuthProtectedAPI = Proxy

type JWTProtectedAPI =
  "userDetails" :> Get '[JSON] User
    :<|> "delete" :> Post '[JSON] ()
    :<|> "auth" :> Get '[JSON] NoContent

-- Used for client
jwtProtectedAPI :: Proxy JWTProtectedAPI
jwtProtectedAPI = Proxy
