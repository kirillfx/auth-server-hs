{-# LANGUAGE TemplateHaskell #-}

module Register where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.SafeCopy
import           Data.Text
import           Relude

data Register = Register
  { rEmail    :: Text,
    rPassword :: Text
  }
  deriving stock (Generic, Eq, Show)

$(deriveJSON defaultOptions ''Register)
$(deriveSafeCopy 0 'base ''Register)
