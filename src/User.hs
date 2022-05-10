module User where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Password.Bcrypt
import           Data.SafeCopy
import           Data.Text            (Text)
import           Data.UUID
import           Relude
import           Servant.Auth.Server

$(deriveSafeCopy 0 'base ''UUID)

data User = User
  { uId           :: UUID,
    uEmail        :: Text,
    uPasswordHash :: Text
  }
  deriving stock (Generic, Eq, Show)

$(deriveJSON defaultOptions ''User)
$(deriveSafeCopy 0 'base ''User)

instance ToJWT User

instance FromJWT User

-- type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult User)

-- instance FromBasicAuthData User where
--   fromBasicAuthData authData authCheckFunction = authCheckFunction authData
