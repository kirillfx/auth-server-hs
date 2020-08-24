{-# LANGUAGE DataKinds #-}

module Server where

import API
import API.Protected
import API.Public
import AppContext
import Control.Monad.Reader
import Crypto.JOSE.JWK (JWK)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant hiding (BasicAuth)
import Servant.Auth.Server
import Server.Protected
import Server.Public
import User

-- Launch with warp
-- All config pieces should be cooked beforehand.
startApp :: JWK -> AppContext -> IO ()
startApp myKey ctx =
  let jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings
      authCfg = authCheck (database ctx) :: BasicAuthCfg
      cfg = cookieCfg :. jwtCfg :. authCfg :. EmptyContext
   in run 8080 (app cfg ctx)

app :: Context context -> AppContext -> Application
app cfg ctx = serve publicApi (mkServer cfg ctx)

-- server :: ServerT API ReaderHandler
-- server = undefined

-- make Public Application
mkServer :: Context context -> AppContext -> Server PublicAPI
mkServer cfg ctx =
  let nt x = runReaderT x ctx
   in hoistServer publicApi nt publicServerT

-- Make Protected

startProtectedApp :: JWK -> AppContext -> IO ()
startProtectedApp myKey ctx =
  let jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings
      authCfg = authCheck (database ctx) :: BasicAuthCfg
      cfg = cookieCfg :. jwtCfg :. authCfg :. EmptyContext
   in run 8080 (protectedApp cfg cookieCfg jwtCfg ctx)

protectedApp ::
  Context
    '[ CookieSettings,
       JWTSettings,
       BasicAuthCfg
     ] ->
  CookieSettings ->
  JWTSettings ->
  AppContext ->
  Application
protectedApp cfg cs jwts ctx = serveWithContext protectedApi cfg (mkProtectedServer cfg cs jwts ctx)

mkProtectedServer ::
  Context
    '[ CookieSettings,
       JWTSettings,
       BasicAuthCfg
     ] ->
  CookieSettings ->
  JWTSettings ->
  AppContext ->
  Server ProtectedAPI
mkProtectedServer cfg cs jwts ctx =
  let nt x = runReaderT x ctx
      cfg' = Proxy :: Proxy '[CookieSettings, JWTSettings, BasicAuthCfg]
   in hoistServerWithContext protectedApi cfg' nt (protectedServerT cs jwts)

-- Combined

startCombinedApp :: JWK -> AppContext -> IO ()
startCombinedApp myKey ctx =
  let jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings
      authCfg = authCheck (database ctx) :: BasicAuthCfg
      cfg = cookieCfg :. jwtCfg :. authCfg :. EmptyContext
   in run 8080 (combinedApp cfg cookieCfg jwtCfg ctx)

combinedApp ::
  Context
    '[ CookieSettings,
       JWTSettings,
       BasicAuthCfg
     ] ->
  CookieSettings ->
  JWTSettings ->
  AppContext ->
  Application
combinedApp cfg cs jwts ctx = serveWithContext api cfg (mkCombinedServer cfg cs jwts ctx)

mkCombinedServer ::
  Context
    '[ CookieSettings,
       JWTSettings,
       BasicAuthCfg
     ] ->
  CookieSettings ->
  JWTSettings ->
  AppContext ->
  Server API
mkCombinedServer cfg cs jwts ctx =
  let nt x = runReaderT x ctx
      cfg' = Proxy :: Proxy '[CookieSettings, JWTSettings, BasicAuthCfg]
   in hoistServerWithContext api cfg' nt (combinedServerT cs jwts)

combinedServerT :: CookieSettings -> JWTSettings -> ServerT API ReaderHandler
combinedServerT cs jwts = publicServerT :<|> (protectedServerT cs jwts)