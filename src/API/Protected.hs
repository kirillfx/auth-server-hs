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

type BasicAuthProtectedAPI = "login" :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] SlimUser)

-- Used for client
basicAuthProtectedAPI :: Proxy (S.BasicAuth "enter point" SlimUser :> BasicAuthProtectedAPI)
basicAuthProtectedAPI = Proxy

type JWTProtectedAPI =
  "userDetails" :> Get '[JSON] User
    :<|> "delete" :> ReqBody '[JSON] Text :> Post '[JSON] ()
    :<|> "auth" :> Get '[JSON] NoContent

-- Used for client
jwtProtectedAPI :: Proxy JWTProtectedAPI
jwtProtectedAPI = Proxy