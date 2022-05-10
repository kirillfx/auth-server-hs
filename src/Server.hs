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

-- Make ServerT from handlers and settings
serverT :: ServerT API App
serverT = publicServerT :<|> protectedServerT
