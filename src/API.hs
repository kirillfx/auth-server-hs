module API
  ( API
  , api
  )
where

import           API.Protected
import           API.Public
import           Relude
import           Servant             hiding (BasicAuth)
import           Servant.Auth.Server
import           AuthToken

type API =
  PublicAPI
    :<|> (Auth '[BasicAuth] AuthToken :> BasicAuthProtectedAPI)
    :<|> (Auth '[JWT, Cookie] AuthToken :> JWTProtectedAPI)

api :: Proxy API
api = Proxy
