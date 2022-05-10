module Env where

import           DB
import           Data.Acid
import           Relude
import           System.Log.FastLogger (LoggerSet)
import Servant.Auth.Server (JWTSettings, CookieSettings)

data Env = Env
  { database  :: AcidState Database
  , getLogger :: LoggerSet
  , jwtSettings :: JWTSettings
  , csSettings :: CookieSettings
  }
