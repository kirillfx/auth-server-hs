module API.Public where

import           API.Types
import           Data.Text           (Text)
import           Login
import           Register
import           Relude
import           Servant             hiding (BasicAuth)
import           Servant.Auth.Server
import           Servant.Client

type PublicAPI =
  "register" :> ReqBody '[JSON] Register :> Post '[JSON] ()
    :<|> "index" :> Get '[JSON] Text

publicApi :: Proxy PublicAPI
publicApi = Proxy
