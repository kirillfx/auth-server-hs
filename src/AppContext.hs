module AppContext where

import DB
import Data.Acid
import System.Log.FastLogger (LoggerSet)

data AppContext = AppContext
  { database :: AcidState Database,
    getLogger :: LoggerSet
  }