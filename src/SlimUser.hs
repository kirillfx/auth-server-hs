{-# LANGUAGE TemplateHaskell #-}

module SlimUser where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Password.Bcrypt
import           Data.SafeCopy
import           Data.Text            (Text)
import           Data.UUID
import           Relude
import           Servant.Auth.Server
import           User                 (User)
import qualified User

newtype SlimUser = SlimUser
  { email    :: Text
  }
  deriving stock (Generic, Eq, Show)

$(deriveJSON defaultOptions ''SlimUser)
$(deriveSafeCopy 0 'base ''SlimUser)

instance ToJWT SlimUser

instance FromJWT SlimUser

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult SlimUser)

instance FromBasicAuthData SlimUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

fromUser :: User -> SlimUser
fromUser user = SlimUser (User.uEmail user)
