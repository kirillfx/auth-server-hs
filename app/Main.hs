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
import Network.Wai.Handler.Warp as Warp
import Servant.Auth.Server
import Server
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.Log.FastLogger

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