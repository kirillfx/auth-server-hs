{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import API.Protected
import API.Public
import AppContext
import Client
import Configuration.Dotenv
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import DB
import Data.Acid
import Data.Aeson
import Data.Either (isRight)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Lib
import Login
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (Header)
import qualified Network.Wai.Handler.Warp as Warp
import Register
import Servant
import Servant.Auth.Server
import Servant.Client
import Server.Protected
import System.Environment (lookupEnv)
import System.Log.FastLogger
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import User

main :: IO ()
main = hspec spec

-- postJson path = request methodPost path headers
--   where
--     headers = [("Content-Type", "application/json")]

-- app' = do
--   bracket
--     (openLocalStateFrom "db" (Database Map.empty))
--     closeAcidState
--     ( \db ->
--         let myKey = fromSecret "asdvndipsvnjivnfisdpvndfvifnifpsvsid"
--          in return $ app myKey (AppContext db)
--     )

-- appState = do
--   db <- openLocalStateFrom "db" (Database Map.empty)
--   return (db, app (AppContext db))

withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action = do
  -- Read dotenv
  -- void $ loadFile defaultConfig

  -- Env lookups
  -- mSecret <- lookupEnv "SECRET"

  bracket
    (openLocalStateFrom "db" (Database Map.empty))
    closeAcidState
    ( \db ->
        do
          appLogger <- newStdoutLoggerSet defaultBufSize
          let myKey = fromSecret "asdvndipsvnjivnfisdpvndfvifnifpsvsid"
              settings = mkSettings (print "Shutting down")
              jwtCfg = defaultJWTSettings myKey
              cookieCfg = defaultCookieSettings
              authCfg = authCheck (database ctx) :: BasicAuthCfg
              cfg = cookieCfg :. jwtCfg :. authCfg :. EmptyContext
              ctx = AppContext db appLogger
              app = mkApplication cfg cookieCfg jwtCfg ctx
          Warp.testWithApplicationSettings settings (pure app) action
    )

spec :: Spec
spec =
  around withUserApp $
    do
      baseUrl <- runIO $ parseBaseUrl "http://localhost"
      manager <- runIO $ newManager defaultManagerSettings
      let basicAuthClient = client (Proxy :: Proxy BasicAuthProtectedAPI)
          clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})
          publicClient = client (Proxy :: Proxy PublicAPI)

      describe
        "/register"
        $ do
          it "responds with 200" $ \port -> do
            let r = Register "kirillfx" "kirillfx@gmail.com" "123"
            res <- runClientM (postRegister r) (clientEnv port)
            liftIO $ print res
            isRight res `shouldBe` True

      describe
        "/login"
        $ do
          it "responds with 200" $ \port -> do
            let l = BasicAuthData "kirillfx@gmail.com" "123"
            res <- runClientM (postLogin l) (clientEnv port)
            -- print res
            isRight res `shouldBe` True

-- describe
--   "/delete"
--   $ do
--     it "responds with 200" $ \port -> do
--       res <- runClientM (postDelete "kirillfx@gmail.com") (clientEnv port)
--       print res
--       isRight res `shouldBe` True