{-# LANGUAGE TemplateHaskell #-}

module Register where

import Data.Aeson
import Data.Aeson.TH
import Data.Text

data Register = Register
  { username :: Text,
    email :: Text,
    password :: Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Register)