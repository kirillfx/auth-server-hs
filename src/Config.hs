module Config
  ( Config(..)
  ) where

import           Data.Aeson
import           Data.Set   (Set)
import           Json
import           Relude

data Config = Config
  { cfgPort     :: Int
  , cfgKeyPath  :: FilePath
  , cfgAcidPath :: FilePath
  , cfgWebhooks :: Set Text
  }
  deriving stock (Generic, Show, Eq)


instance ToJSON Config where
  toJSON = genericToJSON dashedFieldJSONOptions
  toEncoding = genericToEncoding dashedFieldJSONOptions

instance FromJSON Config where
  parseJSON = genericParseJSON dashedFieldJSONOptions
