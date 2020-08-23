{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Protected where

import API.Protected
import API.Types
import AppContext
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import DB
import Data.Acid
import Data.ByteString
import qualified Data.ByteString as BS
import Data.Password.Bcrypt
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.UUID.V4 (nextRandom)
import Login
import Register
import Servant hiding (BasicAuth)
import Servant.Auth.Server
import Servant.Client
import SlimUser
import User

-- Handlers
usersH :: ReaderHandler [SlimUser]
usersH = do
  (AppContext database) <- ask
  us <- liftIO $ query database GetAllUsers
  return (fmap fromUser us)

-- loginH (Login e p) = do
--   liftIO . print $ "Login" <> show e
--   (AppContext database) <- ask
--   eitherUser <- liftIO $ query database (GetUser e p)
--   case eitherUser of
--     Left e -> throwError err500 {errBody = "Can't login"}
--     Right u -> return u

-- deleteH :: Text -> ReaderHandler ()
-- deleteH email = do
--   liftIO $ print $ "Trying to delete " <> email
--   (AppContext database) <- ask
--   eitherDelete <- liftIO $ update database (DeleteUser email)
--   case eitherDelete of
--     Left e -> throwError err500 {errBody = "Can't delete"}
--     Right u -> return u

protectedServerT :: CookieSettings -> JWTSettings -> ServerT ProtectedAPI ReaderHandler
-- protectedServerT _ = throwAll err401 {errBody = "Not Authenticated"}
protectedServerT cs jwts = loginH :<|> userDetailsH
  where
    loginH :: AuthResult User -> ReaderHandler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] User)
    loginH (Authenticated user) = do
      let slimUser = fromUser user
      mApplyCookies <- liftIO $ acceptLogin cs jwts user
      case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> do
          etoken <- liftIO $ makeJWT user jwts Nothing
          case etoken of
            Left e -> throwError err401 {errBody = "Can't make token"}
            Right token -> do
              liftIO $ print token
              return $ applyCookies user
    loginH _ = throwError err401 {errBody = "Can't auth"}
    userDetailsH :: AuthResult User -> ReaderHandler User
    userDetailsH (Authenticated user) = return user
    userDetailsH _ = throwError err401 {errBody = "Can't auth"}

-- Basic auth check function for working with AcidState Database
authCheck :: AcidState Database -> BasicAuthData -> IO (AuthResult User)
authCheck database (BasicAuthData ebs pbs) = do
  let e = decodeUtf8 ebs
      p = decodeUtf8 pbs
  eitherUser <- liftIO $ query database (GetUser e p)
  case eitherUser of
    Left e -> return Indefinite
    Right u -> return (Authenticated u)