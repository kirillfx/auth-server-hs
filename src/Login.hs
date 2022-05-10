module Login where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import           Relude

data Login = Login
  { email    :: Text,
    password :: Text
  }
  deriving (Generic, Eq, Show)

$(deriveJSON defaultOptions ''Login)
