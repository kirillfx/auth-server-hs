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
import Data.ByteString.Lazy as BL
import Data.Password.Bcrypt
import Data.String
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
import SlimUser (SlimUser)
import qualified SlimUser
import System.Log.FastLogger
import User

basicAuthProtectedServer :: CookieSettings -> JWTSettings -> AuthResult SlimUser -> ServerT BasicAuthProtectedAPI ReaderHandler
basicAuthProtectedServer cs jwts (Authenticated user) = loginH
  where
    loginH :: ReaderHandler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] SlimUser)
    loginH = do
      let slimUser = user
      mApplyCookies <- liftIO $ acceptLogin cs jwts slimUser
      case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> do
          etoken <- liftIO $ makeJWT slimUser jwts Nothing
          case etoken of
            Left e -> throwError err401 {errBody = fromString . show $ e}
            Right token -> do
              liftIO $ print token
              return $ applyCookies slimUser
basicAuthProtectedServer cs jwts _ = throwAll err401

jwtProtectedServerT :: CookieSettings -> JWTSettings -> AuthResult SlimUser -> ServerT JWTProtectedAPI ReaderHandler
jwtProtectedServerT cs jwts (Authenticated user) = userDetailsH :<|> deleteUserH :<|> authH
  where
    userDetailsH :: ReaderHandler User
    userDetailsH = do
      (AppContext database logset) <- ask
      eitherUser <- liftIO $ query database (GetUserByEmail . SlimUser.email $ user)
      case eitherUser of
        Left e -> throwError err500 {errBody = fromString e}
        Right u -> return u

    deleteUserH :: Text -> ReaderHandler ()
    deleteUserH email = do
      (AppContext database logset) <- ask
      eitherDelete <- liftIO $ update database (DeleteUser email)
      case eitherDelete of
        Left e -> throwError err500 {errBody = fromString e}
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
authCheck :: AcidState Database -> BasicAuthData -> IO (AuthResult SlimUser)
authCheck database (BasicAuthData ebs pbs) = do
  let e = decodeUtf8 ebs
      p = decodeUtf8 pbs
  eitherUser <- liftIO $ query database (GetUser e p)
  case eitherUser of
    Left e -> return Indefinite
    Right u -> return (Authenticated (SlimUser.fromUser u))