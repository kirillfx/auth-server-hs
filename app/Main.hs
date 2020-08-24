{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppContext
import Control.Exception (bracket)
import Control.Monad (void)
import DB
import Data.Acid
import Lib
import Network.Wai.Handler.Warp (Settings, defaultSettings, setGracefulShutdownTimeout, setInstallShutdownHandler, setPort)
import Servant.Auth.Server
import Server
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTERM)

mkSettings :: IO () -> Settings
mkSettings shutdownAction =
  setPort 8080
    . setGracefulShutdownTimeout (Just 5)
    . setInstallShutdownHandler shutdownHandler
    $ defaultSettings
  where
    shutdownHandler closeSocket = do
      installHandler sigTERM (Catch $ shutdownAction >> closeSocket) Nothing
      void $ installHandler sigINT (Catch $ shutdownAction >> closeSocket) Nothing

main :: IO ()
main = do
  db <- openLocalStateFrom "db" (Database mempty)
  let ctx = AppContext db
      myKey = fromSecret "asdvndipsvnjivnfisdpvndfvifnifpsvsid"
      shutdownAction = print "Shutting down"
      settings = mkSettings shutdownAction
  startApp settings myKey ctx
  closeAcidState db
  print "Acid State closed"
