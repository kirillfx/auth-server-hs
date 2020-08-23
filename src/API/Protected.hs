{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Protected where

import API.Types
import Servant hiding (BasicAuth)
import Servant.Auth.Server
import Servant.Client
import SlimUser
import User

-- Protected API
type ProtectedAPI =
  "login" :> Auth '[BasicAuth] User :> Get '[JSON] SlimUser
    :<|> "userDetails" :> Auth '[BasicAuth] User :> Get '[JSON] User