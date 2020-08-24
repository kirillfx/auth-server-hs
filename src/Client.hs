{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import API
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
-- postRegister :: Register -> ClientM User
-- postLogin :: Login -> ClientM User
-- postDelete :: Text -> ClientM ()
-- (getUsers :<|> postRegister :<|> postLogin :<|> postDelete) = client api