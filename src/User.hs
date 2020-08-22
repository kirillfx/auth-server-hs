{-# LANGUAGE TemplateHaskell #-}

module User where

import Data.Aeson
import Data.Aeson.TH
import Data.Password.Bcrypt
import Data.SafeCopy
import Data.Text (Text)
import Data.UUID

$(deriveSafeCopy 0 'base ''UUID)

data User = User
  { userId :: UUID,
    username :: Text,
    email :: Text,
    passwordHash :: Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
$(deriveSafeCopy 0 'base ''User)