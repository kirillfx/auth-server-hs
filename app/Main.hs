module Main where

import AppContext
import DB
import Data.Acid
import Lib

main :: IO ()
main = do
  db <- openLocalStateFrom "db" (Database mempty)
  let ctx = AppContext db
  startApp ctx
  closeAcidState db
