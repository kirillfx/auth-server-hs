{-# LANGUAGE TemplateHaskell #-}

module Register where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.SafeCopy
import           Data.Text
import           Relude

data Register = Register
  { username :: Text,
    email    :: Text,
    password :: Text
  }
  deriving stock (Generic, Eq, Show)

$(deriveJSON defaultOptions ''Register)
$(deriveSafeCopy 0 'base ''Register)
