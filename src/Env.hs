module Env where

import           DB                    (Database)
import           Data.Acid             (AcidState)
import           Network.HTTP.Client   (Manager, Request)
import           Relude
import           Servant.Auth.Server   (CookieSettings, JWTSettings)
import           System.Log.FastLogger (LoggerSet)


data Env = Env
  { database    :: AcidState Database
  , getLogger   :: LoggerSet
  , jwtSettings :: JWTSettings
  , csSettings  :: CookieSettings
  , webhooks    :: [Request]
  , manager     :: Manager
  } deriving Generic
