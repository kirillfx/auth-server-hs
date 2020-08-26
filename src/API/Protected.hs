{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Protected where

import API.Types
import Data.ByteString
import Data.Text (Text)
import Servant hiding (BasicAuth)
import qualified Servant as S
import Servant.Auth.Server
import Servant.Client
import SlimUser
import User

-- Protected API
-- type ProtectedAPI =
--   "login" :> Auth '[BasicAuth] User :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] User)
--     :<|> "userDetails" :> Auth '[JWT, Cookie] User :> Get '[JSON] User
--     :<|> "delete" :> Auth '[JWT, Cookie] User :> ReqBody '[JSON] Text :> Post '[JSON] ()
--     :<|> "auth" :> Auth '[JWT, Cookie] User :> Get '[JSON] NoContent

type BasicAuthProtectedAPI = "login" :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] User)

-- Used for client
basicAuthProtectedAPI :: Proxy (S.BasicAuth "enter point" User :> BasicAuthProtectedAPI)
basicAuthProtectedAPI = Proxy

type JWTProtectedAPI =
  "userDetails" :> Get '[JSON] User
    :<|> "delete" :> ReqBody '[JSON] Text :> Post '[JSON] ()
    :<|> "auth" :> Get '[JSON] NoContent