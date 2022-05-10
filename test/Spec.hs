

module Main (main) where

import           API.Protected
import           API.Public
import           AppContext
import           Client
import           Configuration.Dotenv
import           Control.Exception
import           Control.Monad               (forM_)
import           Control.Monad.IO.Class      (liftIO)
import           DB
import           Data.Acid
import           Data.Aeson
import           Data.Either                 (isRight)
import qualified Data.Map                    as Map
import qualified Data.Text                   as T
import           Data.UUID.V4                (nextRandom)
import           Lib
import           Network.HTTP.Client         hiding (Proxy)
import           Network.HTTP.Types          (methodPost)
import           Network.HTTP.Types.Header   (Header)
import qualified Network.Wai.Handler.Warp    as Warp
import           Register
import           Relude
import           Servant
import           Servant.API.ResponseHeaders
import           Servant.Auth.Server
import           Servant.Client
import           Server.Protected
import qualified SlimUser
import           System.Directory            (removeDirectoryRecursive)
import           System.Environment          (lookupEnv)
import           System.Log.FastLogger
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           User

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
    (openLocalStateFrom "test_db" (Database Map.empty))
    closeAcidState
    ( \db ->
        do
          appLogger <- newStdoutLoggerSet defaultBufSize
          let myKey = fromSecret "asdvndipsvnjivnfisdpvndfvifnifpsvsid"
              settings = mkSettings (print "Shutting down")
              jwtCfg = defaultJWTSettings myKey
              cookieCfg = defaultCookieSettings {cookieIsSecure = NotSecure}
              authCfg = authCheck (database ctx) :: BasicAuthCfg
              cfg = cookieCfg :. jwtCfg :. authCfg :. EmptyContext
              ctx = AppContext db appLogger
              app = mkApplication cfg cookieCfg jwtCfg ctx
          Warp.testWithApplicationSettings settings (pure app) action
    )

spec :: Spec
spec =
  afterAll_ (removeDirectoryRecursive "./test_db") $
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
              -- print $ getResponse res
              isRight res `shouldBe` True

        describe
          "/delete"
          $ do
            it "responds with 200" $ \port -> do
              let l = BasicAuthData "kirillfx@gmail.com" "123"
              eLoginResponse <- runClientM (postLogin l) (clientEnv port)
              case eLoginResponse of
                Left e -> expectationFailure "login failed"
                Right hs -> do
                  let u = getResponse hs
                      hs' = getHeaders hs
                  print u
                  print hs'
                  (SlimUser.email u) `shouldBe` "kirillfx@gmail.com"

-- case loginResponse of
--   Left e -> expectationFailure "login failed"
--   Right u -> do
--     -- let token = lookupResponseHeader (loginResponse :: ResponseHeader "Set-Cookie" String)
--     -- print token
--     -- print (getHeaders loginResponse)
--     -- res <- runClientM (postDelete "kirillfx@gmail.com") (clientEnv port)
--     -- print res
--     isRight u `shouldBe` True
