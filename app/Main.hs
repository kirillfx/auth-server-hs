{-# LANGUAGE OverloadedStrings #-}

module Main where

import AppContext
import Configuration.Dotenv
import Control.Exception (bracket)
import Control.Monad (void)
import DB
import Data.Acid
import Data.String (fromString)
import Data.Time.Clock (getCurrentTime)
import Lib
import Logging
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setGracefulShutdownTimeout, setInstallShutdownHandler, setPort)
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Servant.Auth.Server
import Server
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.Log.FastLogger
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTERM)

jsonRequestLogger :: IO Middleware
-- jsonRequestLogger =
--   mkRequestLogger $ def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}
jsonRequestLogger =
  return logStdout

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
      -- Loggers setup
      warpLogger <- jsonRequestLogger
      appLogger <- newStdoutLoggerSet defaultBufSize

      -- Startup log event
      tstamp <- getCurrentTime
      let lgmsg =
            LogMessage
              { message = "Auth server starting!",
                timestamp = tstamp,
                level = "info"
              }
      pushLogStrLn appLogger (toLogStr lgmsg) >> flushLogStr appLogger

      let shutdownAction = print "Shutting down"
          settings = mkSettings shutdownAction

      bracket
        (openLocalStateFrom "db" (Database mempty))
        (\db -> closeAcidState db >> print "Acid State closed")
        ( \db ->
            -- Constructing AppContext
            let ctx = AppContext db appLogger
             in -- Starting Server
                startApp warpLogger settings (fromSecret (fromString myKey)) ctx
        )
      exitSuccess