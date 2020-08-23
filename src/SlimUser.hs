{-# LANGUAGE TemplateHaskell #-}

module SlimUser where

import Data.Aeson
import Data.Aeson.TH
import Data.Password.Bcrypt
import Data.SafeCopy
import Data.Text (Text)
import Data.UUID
import User (User)
import qualified User

data SlimUser = SlimUser
  { username :: Text,
    email :: Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''SlimUser)
$(deriveSafeCopy 0 'base ''SlimUser)

fromUser :: User -> SlimUser
fromUser user = SlimUser (User.username user) (User.email user)