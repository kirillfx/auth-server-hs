{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API
  ( API,
    api,
    ReaderHandler,
  )
where

import API.Protected
import API.Public
import API.Types
import Servant hiding (BasicAuth)
import Servant.Auth.Server
import User

type API =
  PublicAPI
    :<|> (Auth '[BasicAuth] User :> BasicAuthProtectedAPI)
    :<|> (Auth '[JWT, Cookie] User :> JWTProtectedAPI)

api :: Proxy API
api = Proxy