{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import API
import Network.Wai
import Network.Wai.Handler.Warp
import Register
import SlimUser
import User

-- Protected API
-- type Protected =
--   "loginb" :> Auth '[BasicAuth] User :> Get '[JSON] User
--     :<|> "userDetails" :> Auth '[BasicAuth] User :> Get '[JSON] User

-- protected :: AuthResult User -> ServerT Protected ReaderHandler
-- protected (Authenticated user) = loginbH :<|> userDetailsH
--   where
--     loginbH :: AuthResult User -> ReaderHandler User
--     loginbH (Authenticated user) = undefined
--     loginbH _ = undefined

--     userDetailsH :: AuthResult User -> ReaderHandler User
--     userDetailsH (Authenticated user) = undefined
--     userDetailsH _ = undefined
