module AppContext where

import DB
import Data.Acid

data AppContext = AppContext
  { db :: AcidState Database
  }