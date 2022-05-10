module Server where

import           API
import           Network.Wai
import           Network.Wai.Handler.Warp (Settings, runSettings)
import           Relude
import           Servant                  hiding (BasicAuth)
import           Servant.Auth.Server
import           Server.Protected
import           Server.Public

-- Make ServerT from handlers and settings
serverT :: CookieSettings -> JWTSettings -> ServerT API ReaderHandler
serverT cs jwts = publicServerT :<|> protectedServerT cs jwts
