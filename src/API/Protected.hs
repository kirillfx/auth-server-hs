module API.Protected where

import           Data.ByteString
import           Data.Text           (Text)
import           Relude
import           Servant             hiding (BasicAuth)
import qualified Servant             as S
import           Servant.Auth.Server
import Servant.API.Generic
import           Servant.Client
import           AuthToken 
import           User

type BasicAuthProtectedAPI = "login" :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthToken)

newtype BasicAuthProtectedRoutes route = BasicAuthProtectedRoutes
  { _login :: route :- BasicAuthProtectedAPI
  } deriving stock Generic


-- Used for client
basicAuthProtectedAPI :: Proxy (S.BasicAuth "enter point" AuthToken :> BasicAuthProtectedAPI)
basicAuthProtectedAPI = Proxy


data JWTProtectedRoutes route = JWTProtectedRoutes
  { _userDetails :: route :- "user" :> Get '[JSON] User
  , _delete :: route :- "user" :> Delete '[JSON] NoContent
  , _auth :: route :- "auth" :> Get '[JSON] NoContent
  } deriving stock Generic


type JWTProtectedAPI =
  "userDetails" :> Get '[JSON] User
    :<|> "delete" :> Post '[JSON] ()
    :<|> "auth" :> Get '[JSON] NoContent


-- Used for client
jwtProtectedAPI :: Proxy JWTProtectedAPI
jwtProtectedAPI = Proxy
