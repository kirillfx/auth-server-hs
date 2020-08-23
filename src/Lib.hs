{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
    API,
    getUsers,
    postRegister,
    postLogin,
    postDelete,
  )
where

import AppContext
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import DB
import Data.Acid
import Data.Password.Bcrypt
import Data.Text (Text)
import Data.UUID.V4 (nextRandom)
import Login (Login (..))
import qualified Login
import Network.Wai
import Network.Wai.Handler.Warp
import Register
import Servant
import Servant.Client
import SlimUser
import User

type ReaderHandler = ReaderT AppContext Handler

type API =
  "users" :> Get '[JSON] [SlimUser]
    :<|> "register" :> ReqBody '[JSON] Register :> Post '[JSON] User
    :<|> "login" :> ReqBody '[JSON] Login :> Post '[JSON] User
    :<|> "delete" :> ReqBody '[JSON] Text :> Post '[JSON] ()

api :: Proxy API
api = Proxy

-- Launch with warp
startApp :: AppContext -> IO ()
startApp ctx = run 8080 (app ctx)

app :: AppContext -> Application
app ctx = serve api (server ctx)

-- Handlers
users :: ReaderHandler [SlimUser]
users = do
  (AppContext database) <- ask
  us <- liftIO $ query database GetAllUsers
  return (fmap fromUser us)

register :: Register -> ReaderHandler User
register r@(Register u e p) = do
  liftIO . print $ "Register" <> show r
  i <- liftIO nextRandom
  p' <- liftIO $ hashPassword (mkPassword p)
  let newUser = User i u e (unPasswordHash p')
  (AppContext database) <- ask
  eitherUser <- liftIO $ update database (RegisterUser newUser)
  case eitherUser of
    Left e -> throwError err500 {errBody = "Can't register"}
    Right u -> return u

login :: Login -> ReaderHandler User
login (Login e p) = do
  liftIO . print $ "Login" <> show e
  (AppContext database) <- ask
  eitherUser <- liftIO $ query database (GetUser e p)
  case eitherUser of
    Left e -> throwError err500 {errBody = "Can't login"}
    Right u -> return u

delete :: Text -> ReaderHandler ()
delete email = do
  liftIO $ print $ "Trying to delete " <> email
  (AppContext database) <- ask
  eitherDelete <- liftIO $ update database (DeleteUser email)
  case eitherDelete of
    Left e -> throwError err500 {errBody = "Can't delete"}
    Right u -> return u

-- make Application
server :: AppContext -> Server API
server ctx =
  let nt x = runReaderT x ctx
   in hoistServer api nt readerServerT

readerServerT :: ServerT API ReaderHandler
readerServerT = users :<|> register :<|> login :<|> delete

-- Servant Client
getUsers :: ClientM [SlimUser]
postRegister :: Register -> ClientM User
postLogin :: Login -> ClientM User
postDelete :: Text -> ClientM ()
(getUsers :<|> postRegister :<|> postLogin :<|> postDelete) = client api
