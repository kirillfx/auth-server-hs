module API.Public where

import           Login
import           Register
import           Relude
import           Servant             hiding (BasicAuth)
import           Servant.Auth.Server
import           Servant.Client
import Servant.API.Generic


type PublicAPI =
  "register" :> ReqBody '[JSON] Register :> Post '[JSON] ()
    :<|> "index" :> Get '[JSON] Text


data PublicRoutes route = PublicRoutes
  { _register :: route :- "register" :> ReqBody '[JSON] Register :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Text)
  , _index :: route :- "index" :> Get '[JSON] Text
  }
  deriving stock Generic


publicApi :: Proxy PublicAPI
publicApi = Proxy
