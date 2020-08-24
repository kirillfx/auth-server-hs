module API.Types where

import AppContext
import Control.Monad.Reader
import Servant hiding (BasicAuth)

type ReaderHandler = ReaderT AppContext Handler