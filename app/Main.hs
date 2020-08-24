{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppContext
import DB
import Data.Acid
import Lib
import Servant.Auth.Server
import Server

main :: IO ()
main = do
  db <- openLocalStateFrom "db" (Database mempty)
  let ctx = AppContext db
      myKey = fromSecret "asdvndipsvnjivnfisdpvndfvifnifpsvsid"
  startApp myKey ctx
  closeAcidState db
