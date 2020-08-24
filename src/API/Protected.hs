{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Protected where

import API.Types
import Data.ByteString
import Data.Text (Text)
import Servant hiding (BasicAuth)
import Servant.Auth.Server
import Servant.Client
import SlimUser
import User

-- Protected API
type ProtectedAPI =
  "login" :> Auth '[BasicAuth] User :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] User)
    :<|> "userDetails" :> Auth '[JWT, Cookie] User :> Get '[JSON] User
    :<|> "delete" :> Auth '[JWT, Cookie] User :> ReqBody '[JSON] Text :> Post '[JSON] ()

protectedApi :: Proxy ProtectedAPI
protectedApi = Proxy