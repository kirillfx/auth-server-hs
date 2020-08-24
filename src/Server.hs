{-# LANGUAGE DataKinds #-}

module Server where

import API
import API.Protected
import API.Public
import AppContext
import Control.Monad.Reader
import Crypto.JOSE.JWK (JWK)
import Network.Wai
import Network.Wai.Handler.Warp (Settings, runSettings)
import Servant hiding (BasicAuth)
import Servant.Auth.Server
import Server.Protected
import Server.Public
import User

startApp :: Middleware -> Settings -> JWK -> AppContext -> IO ()
startApp loggingMiddleware settings myKey ctx =
  -- Servant context assembly
  let jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings
      authCfg = authCheck (database ctx) :: BasicAuthCfg
      cfg = cookieCfg :. jwtCfg :. authCfg :. EmptyContext
   in runSettings settings $ loggingMiddleware $ (mkApplication cfg cookieCfg jwtCfg ctx) -- run

-- Make Wai.Application from parts
mkApplication ::
  Context
    '[ CookieSettings,
       JWTSettings,
       BasicAuthCfg
     ] ->
  CookieSettings ->
  JWTSettings ->
  AppContext ->
  Application
mkApplication cfg cs jwts ctx = serveWithContext api cfg (mkServer cfg cs jwts ctx)

-- Make Servant Server
mkServer ::
  Context
    '[ CookieSettings,
       JWTSettings,
       BasicAuthCfg
     ] ->
  CookieSettings ->
  JWTSettings ->
  AppContext ->
  Server API
mkServer cfg cs jwts ctx =
  let nt x = runReaderT x ctx
      cfg' = Proxy :: Proxy '[CookieSettings, JWTSettings, BasicAuthCfg]
   in hoistServerWithContext api cfg' nt (serverT cs jwts)

-- Make ServerT from handlers and settings
serverT :: CookieSettings -> JWTSettings -> ServerT API ReaderHandler
serverT cs jwts = publicServerT :<|> (protectedServerT cs jwts)