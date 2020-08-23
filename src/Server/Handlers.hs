{-# LANGUAGE OverloadedStrings #-}

module Server.Handlers where

import API
import AppContext
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import DB
import Data.Acid
import Data.Password.Bcrypt
import Data.Text (Text)
import Login (Login (..))
import qualified Login
import Network.Wai
import Register
import Servant
import Servant.Server
import SlimUser
import User
