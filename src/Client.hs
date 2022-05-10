module Client where

import           API
import           API.Protected
import           API.Public
import           Data.Text
import           Login
import           Register
import           Relude
import           Servant
import           Servant.Auth.Server
import           Servant.Client
import           SlimUser
import           User

(postRegister :<|> getIndex) = client publicApi

postLogin = client basicAuthProtectedAPI

(getUserDetails :<|> postDelete :<|> getAuth) = client jwtProtectedAPI
