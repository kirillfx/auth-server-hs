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
import Servant
import Servant.Auth.Server

type API = PublicAPI :<|> ProtectedAPI

api :: Proxy API
api = Proxy