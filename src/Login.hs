{-# LANGUAGE TemplateHaskell #-}

module Login where

import Data.Aeson
import Data.Aeson.TH
import Data.Text

data Login = Login
  { email :: Text,
    password :: Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Login)