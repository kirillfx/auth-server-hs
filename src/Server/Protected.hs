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
import Data.Password.Bcrypt
import Data.Text (Text)
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

loginH :: AuthResult User -> ReaderHandler SlimUser
loginH _ = undefined

-- loginH (Login e p) = do
--   liftIO . print $ "Login" <> show e
--   (AppContext database) <- ask
--   eitherUser <- liftIO $ query database (GetUser e p)
--   case eitherUser of
--     Left e -> throwError err500 {errBody = "Can't login"}
--     Right u -> return u

userDetailsH :: AuthResult User -> ReaderHandler User
userDetailsH _ = undefined

deleteH :: Text -> ReaderHandler ()
deleteH email = do
  liftIO $ print $ "Trying to delete " <> email
  (AppContext database) <- ask
  eitherDelete <- liftIO $ update database (DeleteUser email)
  case eitherDelete of
    Left e -> throwError err500 {errBody = "Can't delete"}
    Right u -> return u

protectedServerT :: ServerT ProtectedAPI ReaderHandler
protectedServerT = loginH :<|> userDetailsH