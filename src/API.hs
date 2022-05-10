module API
  ( API,
    api,
    ReaderHandler,
  )
where

import           API.Protected
import           API.Public
import           API.Types
import           Relude
import           Servant             hiding (BasicAuth)
import           Servant.Auth.Server
import           SlimUser

type API =
  PublicAPI
    :<|> (Auth '[BasicAuth] SlimUser :> BasicAuthProtectedAPI)
    :<|> (Auth '[JWT, Cookie] SlimUser :> JWTProtectedAPI)

api :: Proxy API
api = Proxy
