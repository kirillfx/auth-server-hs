module Config
  ( Config(..)
  ) where

import Data.Aeson
import Relude
import Json
import Data.Set (Set)

data Config = Config
  { cfgPort :: Int
  , cfgKeyPath :: FilePath
  , cfgAcidPath :: FilePath
  , cfgWebHooks :: Set Text
  }
  deriving stock (Generic, Show, Eq)


instance ToJSON Config where
  toJSON = genericToJSON dashedFieldJSONOptions
  toEncoding = genericToEncoding dashedFieldJSONOptions

instance FromJSON Config where
  parseJSON = genericParseJSON dashedFieldJSONOptions
