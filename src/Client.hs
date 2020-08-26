{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import API
import API.Protected
import API.Public
import Data.Text
import Login
import Register
import Servant
import Servant.Auth.Server
import Servant.Client
import SlimUser
import User

-- Servant Client
-- getUsers :: ClientM [SlimUser]
-- postLogin :: Login -> ClientM User
-- postDelete :: Text -> ClientM ()
-- (getUsers :<|> postRegister :<|> postLogin :<|> postDelete) = client api

-- postRegister :: Register -> ClientM User
-- getIndex :: ClientM Text
(postRegister :<|> getIndex) = client publicApi

postLogin = client basicAuthProtectedAPI