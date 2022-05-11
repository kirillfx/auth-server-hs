module API
  ( AuthServiceRoutes(..)
  , api
  )
where

import           API.Protected
import           API.Public
import           AuthToken
import           Relude
import           Servant             hiding (BasicAuth)
import           Servant.API.Generic (ToServant, ToServantApi, type (:-))
import           Servant.Auth.Server

type API =
  PublicAPI
    :<|> (Auth '[BasicAuth] AuthToken :> BasicAuthProtectedAPI)
    :<|> (Auth '[JWT, Cookie] AuthToken :> JWTProtectedAPI)


data AuthServiceRoutes route = AuthServiceRoutes
  { _public :: route :- ToServantApi PublicRoutes
  , _login :: route :- Auth '[BasicAuth] AuthToken :> ToServantApi BasicAuthProtectedRoutes
  , _jwtProtected :: route :- Auth '[JWT, Cookie] AuthToken :> ToServantApi JWTProtectedRoutes
  } deriving stock Generic


-- api :: Proxy API
api :: Proxy (ToServantApi AuthServiceRoutes)
api = Proxy
