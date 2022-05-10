{-# LANGUAGE TemplateHaskell #-}

module AuthToken where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Password.Bcrypt
import           Data.SafeCopy
import           Data.Text            (Text)
import           Data.UUID
import           Relude hiding (toText)
import           Servant.Auth.Server
import           User                 (User)
import qualified User
import Control.Lens
import           Data.Generics.Labels


newtype AuthToken = AuthToken
  { userId    :: UUID
  }
  deriving stock (Generic, Eq, Show)


$(deriveJSON defaultOptions ''AuthToken)
$(deriveSafeCopy 0 'base ''AuthToken)


instance ToJWT AuthToken
instance FromJWT AuthToken


type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthToken)


instance FromBasicAuthData AuthToken where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

fromUser :: User -> AuthToken
fromUser user = AuthToken (user ^. #uId)
