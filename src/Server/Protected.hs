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
import Data.Time.Clock (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Logging
import Login
import Register
import Servant hiding (BasicAuth)
import Servant.Auth.Server
import Servant.Client
import SlimUser
import System.Log.FastLogger
import User

-- Handlers
usersH :: ReaderHandler [SlimUser]
usersH = do
  (AppContext database logger) <- ask
  us <- liftIO $ query database GetAllUsers
  return (fmap fromUser us)

basicAuthProtectedServer :: CookieSettings -> JWTSettings -> AuthResult User -> ServerT BasicAuthProtectedAPI ReaderHandler
basicAuthProtectedServer cs jwts (Authenticated user) = undefined
  where
    loginH = undefined
basicAuthProtectedServer cs jwts _ = throwAll err401

jwtProtectedServerT :: CookieSettings -> JWTSettings -> AuthResult User -> ServerT JWTProtectedAPI ReaderHandler
jwtProtectedServerT cs jwts (Authenticated user) = userDetailsH :<|> deleteUserH :<|> authH
  where
    userDetailsH :: ReaderHandler User
    userDetailsH = return user

    deleteUserH :: Text -> ReaderHandler ()
    deleteUserH email = do
      (AppContext database logset) <- ask
      eitherDelete <- liftIO $ update database (DeleteUser email)
      case eitherDelete of
        Left e -> throwError err500 {errBody = "Can't delete"}
        Right u -> do
          tstamp <- liftIO $ getCurrentTime
          let logMsg =
                LogMessage
                  { message = email <> " deleted",
                    timestamp = tstamp,
                    level = "info"
                  }
          liftIO $ pushLogStrLn logset $ toLogStr logMsg
          return ()

    authH :: ReaderHandler NoContent
    authH = return NoContent
jwtProtectedServerT cs jwts _ = throwAll err401

protectedServerT cs jwts = basicAuthProtectedServer cs jwts :<|> jwtProtectedServerT cs jwts

-- Basic auth check function for working with AcidState Database
authCheck :: AcidState Database -> BasicAuthData -> IO (AuthResult User)
authCheck database (BasicAuthData ebs pbs) = do
  let e = decodeUtf8 ebs
      p = decodeUtf8 pbs
  eitherUser <- liftIO $ query database (GetUser e p)
  case eitherUser of
    Left e -> return Indefinite
    Right u -> return (Authenticated u)