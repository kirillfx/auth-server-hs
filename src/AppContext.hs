module AppContext where

import DB
import Data.Acid

data AppContext = AppContext
  { database :: AcidState Database
  }