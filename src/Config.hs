module Config
  ( Config(..)
  ) where

import Data.Aeson
import Relude
import Json

data Config = Config
  { cfgPort :: Int
  , cfgKeyPath :: FilePath
  , cfgAcidPath :: FilePath
  }
  deriving stock (Generic, Show, Eq)


instance ToJSON Config where
  toJSON = genericToJSON dashedFieldJSONOptions
  toEncoding = genericToEncoding dashedFieldJSONOptions

instance FromJSON Config where
  parseJSON = genericParseJSON dashedFieldJSONOptions
