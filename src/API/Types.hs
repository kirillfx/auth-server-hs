module API.Types where

import AppContext
import Control.Monad.Reader
import Servant hiding (BasicAuth)

-- | Servant App Monad Transformer.
type ReaderHandler = ReaderT AppContext Handler