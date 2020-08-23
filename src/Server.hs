module Server where

import API
import API.Public
import AppContext
import Control.Monad.Reader
import Crypto.JOSE.JWK (JWK)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant hiding (BasicAuth)
import Servant.Auth.Server
import Server.Handlers
import Server.Public

-- Launch with warp
-- All config pieces should be cooked beforehand.
startApp :: JWK -> AppContext -> IO ()
startApp myKey ctx =
  let jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings
      cfg = cookieCfg :. jwtCfg :. EmptyContext
   in run 8080 (app cfg ctx)

app :: Context context -> AppContext -> Application
app cfg ctx = serve publicApi (mkServer cfg ctx)

server :: ServerT API ReaderHandler
server = undefined

-- make Application
mkServer :: Context context -> AppContext -> Server PublicAPI
mkServer cfg ctx =
  let nt x = runReaderT x ctx
   in hoistServer publicApi nt publicServerT