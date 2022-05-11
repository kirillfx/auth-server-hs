module Server where

import           API
import           Network.Wai
import           Network.Wai.Handler.Warp (Settings, runSettings)
import           Relude
import           Servant                  hiding (BasicAuth)
import           Servant.Auth.Server
import           Server.Protected
import           Server.Public
import App
import Servant.Server.Generic (genericServerT, AsServerT)
import Servant.API.Generic (ToServant)


serverT :: ToServant AuthServiceRoutes (AsServerT App)
serverT =
  genericServerT $
    AuthServiceRoutes
      { _public = publicServerT
      , _login = loginH -- ^ BaiscAuth protected endpoint 
      , _jwtProtected = jwtProtectedServerT
      }
