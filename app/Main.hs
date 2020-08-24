{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppContext
import Configuration.Dotenv
import Control.Exception (bracket)
import Control.Monad (void)
import DB
import Data.Acid
import Data.String (fromString)
import Lib
import Network.Wai.Handler.Warp (Settings, defaultSettings, setGracefulShutdownTimeout, setInstallShutdownHandler, setPort)
import Servant.Auth.Server
import Server
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
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
  -- Read dotenv
  void $ loadFile defaultConfig

  -- Env lookups
  mSecret <- lookupEnv "SECRET"

  case mSecret of
    -- Exit with failure
    Nothing -> exitFailure
    -- Launch server
    Just myKey -> do
      let shutdownAction = print "Shutting down"
          settings = mkSettings shutdownAction
      bracket
        (openLocalStateFrom "db" (Database mempty))
        (\db -> closeAcidState db >> print "Acid State closed")
        ( \db ->
            let ctx = AppContext db
             in startApp settings (fromSecret (fromString myKey)) ctx
        )
      exitSuccess