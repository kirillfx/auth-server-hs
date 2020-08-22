{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
where

import AppContext
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Text (Text)
import Login
import Network.Wai
import Network.Wai.Handler.Warp
import Register
import Servant
import User

type ReaderHandler = ReaderT AppContext Handler

type API =
  "users" :> Get '[JSON] [User]
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
users :: ReaderHandler [User]
users = undefined

register :: Register -> ReaderHandler User
register r = do
  liftIO . print $ "Register" <> show r
  return undefined

login :: Login -> ReaderHandler User
login l = do
  liftIO . print $ "Login " <> show l
  return undefined

delete :: Text -> ReaderHandler ()
delete email = do
  liftIO $ print $ "Trying to delete " <> email
  return undefined

-- make Application
server :: AppContext -> Server API
server ctx =
  let nt x = runReaderT x ctx
   in hoistServer api nt readerServerT

readerServerT :: ServerT API ReaderHandler
readerServerT = users :<|> register :<|> login :<|> delete

-- users_list :: [User]
-- users_list =
--   [ User 1 "Isaac" "isaac@gmail.com" "123",
--     User 2 "Albert" "albert@gmail.com" "456"
--   ]
