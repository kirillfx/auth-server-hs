{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import AppContext
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
import Servant.Client
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import User

main :: IO ()
main = hspec spec

postJson path = request methodPost path headers
  where
    headers = [("Content-Type", "application/json")]

-- users_list :: IO [User]
-- users_list =
--   u1 <- fmap (\i -> User i "Isaac" "isaac@gmail.com" "123") nextRandom
--   [ ,
--     User 2 "Albert" "albert@gmail.com" "456"
--   ]

-- bodyMatcher :: [Header] -> Body -> Maybe String
-- bodyMatcher _ body = case (decode body :: Maybe [User]) of
--   Just xs -> if xs == users_list then Nothing else Just "Wrong output"
--   _ -> Just "This is how we represent failure: this message will be printed"

-- users = encode users_list

app' = do
  bracket
    (openLocalStateFrom "db" (Database Map.empty))
    closeAcidState
    ( \db ->
        return $ app (AppContext db)
    )

appState = do
  db <- openLocalStateFrom "db" (Database Map.empty)
  return (db, app (AppContext db))

withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action =
  bracket
    (openLocalStateFrom "db" (Database Map.empty))
    closeAcidState
    ( \db ->
        Warp.testWithApplication (pure $ app (AppContext db)) action
    )

spec :: Spec
spec =
  around withUserApp $
    do
      let apiClient = client (Proxy :: Proxy API)
      baseUrl <- runIO $ parseBaseUrl "http://localhost"
      manager <- runIO $ newManager defaultManagerSettings
      let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})
      describe
        "/getUsers"
        $ do
          it "responds with 200" $ \port -> do
            res <- runClientM getUsers (clientEnv port)
            -- get "/users" `shouldRespondWith` 200
            liftIO $ print res
            isRight res `shouldBe` True

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
            let l = Login "kirillfx@gmail.com" "123"
            res <- runClientM (postLogin l) (clientEnv port)
            print res
            isRight res `shouldBe` True

      describe
        "/delete"
        $ do
          it "responds with 200" $ \port -> do
            res <- runClientM (postDelete "kirillfx@gmail.com") (clientEnv port)
            print res
            isRight res `shouldBe` True