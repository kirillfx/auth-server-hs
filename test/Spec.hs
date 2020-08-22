{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import AppContext
import Control.Monad.IO.Class (liftIO)
import DB
import Data.Acid
import Data.Aeson
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Lib (app)
import Login
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (Header)
import Register
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

spec :: Spec
spec = do
  -- Prepare
  db <- liftIO $ openLocalStateFrom "db" (Database Map.empty)
  let ctx = AppContext db
      app' = app ctx
  -- HSPEC
  with (return app') $ do
    describe "GET /users" $ do
      it "responds with 200" $ do
        get "/users" `shouldRespondWith` 200
    -- it "responds with [User]" $ do
    --   -- let users = "[{\"userId\":1,\"userFirstName\":\"Isaac\",\"userLastName\":\"Newton\"},{\"userId\":2,\"userFirstName\":\"Albert\",\"userLastName\":\"Einstein\"}]"
    --   get "/users" `shouldRespondWith` 200 {matchBody = MatchBody bodyMatcher} -- {matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]}
    describe "POST /register" $ do
      it "responds with 200" $ do
        let bs = encode (Register "kirillfx" "kirillfx@gmail.com" "123")
        liftIO $ print bs
        postJson "/register" bs
          `shouldRespondWith` 200

    describe "POST /login" $ do
      it "responds with 200" $ do
        let bs = encode (Login "kirillfx@gmail.com" "123")
        liftIO $ print bs
        postJson "/login" bs
          `shouldRespondWith` 200

    describe "POST /delete" $ do
      it "responds with 200" $ do
        let bs = encode $ T.pack "kirillfx@gmail.com"
        postJson "/delete" bs `shouldRespondWith` 200

  liftIO $ closeAcidState db
  where
    app' = do
