{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import API
import AppContext
import Control.Monad (void)
import Control.Monad.Reader
import Crypto.JOSE.JWK (JWK)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp (Settings, defaultSettings, setGracefulShutdownTimeout, setInstallShutdownHandler, setPort)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Register
import Servant
import Servant.Auth.Server
import Server (serverT)
import Server.Protected
import Server.Public
import SlimUser
import System.Log.FastLogger
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTERM)
import User

-- | Construct json logger
jsonRequestLogger :: IO Middleware
jsonRequestLogger =
  mkRequestLogger $ def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}

-- | Construct simple stdout logger
stdOutLogger :: IO Middleware
stdOutLogger = return logStdout

-- | Make Wai Settings that handles app termination signals with provided shutdownAction
mkSettings :: IO () -> Settings
mkSettings shutdownAction =
  setPort 8080
    . setGracefulShutdownTimeout (Just 5)
    . setInstallShutdownHandler shutdownHandler
    $ defaultSettings
  where
    shutdownHandler closeSocket = do
      installHandler sigTERM (Catch $ shutdownAction >> closeSocket) Nothing
      void $ installHandler sigINT (Catch $ shutdownAction >> closeSocket) Nothing

-- run
startApp :: Middleware -> Settings -> JWK -> AppContext -> IO ()
startApp loggingMiddleware settings myKey ctx =
  -- Servant context assembly
  let jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings {cookieIsSecure = NotSecure}
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