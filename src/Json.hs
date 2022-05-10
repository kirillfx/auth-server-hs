
-- | JSON utilities for c-lightning REST API.
module Json
  ( underscoredOptions,
    dashedFieldJSONOptions
  )
where

import Data.Aeson
import Data.Char
import Relude


underscoredOptions :: Options
underscoredOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . dropWhile (not . isUpper)
    }


dashedFieldJSONOptions :: Options
dashedFieldJSONOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '-' . dropWhile (not . isUpper)
    }
