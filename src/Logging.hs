module Logging where

import           Data.Aeson
import           Data.Text                                 (Text)
import           Data.Time.Clock                           (UTCTime)
import           GHC.Generics
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON
import           Relude
import           System.Log.FastLogger

data LogMessage = LogMessage
  { message   :: !Text,
    timestamp :: !UTCTime,
    level     :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON LogMessage

instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode
