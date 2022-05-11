{-# LANGUAGE TemplateHaskell #-}

module Register where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.SafeCopy
import           Data.Text
import           Relude
import Json (underscoredOptions)

data Register = Register
  { rEmail    :: Text,
    rPassword :: Text
  }
  deriving stock (Generic, Eq, Show)

instance ToJSON Register where
  toJSON = genericToJSON underscoredOptions
  toEncoding = genericToEncoding underscoredOptions

instance FromJSON Register where
  parseJSON = genericParseJSON underscoredOptions
  

$(deriveSafeCopy 0 'base ''Register)
