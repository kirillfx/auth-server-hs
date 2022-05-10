module Application where

import           API
import           Env
import           AuthToken
import           Control.Exception                         (bracket)
import           Control.Monad                             (void)
import           Control.Monad.Reader
import           Crypto.JOSE.JWK                           (JWK)
import           DB
import           Data.Acid
import           Data.String                               (fromString)
import           Data.Time.Clock                           (getCurrentTime)
import           Env
import           Logging
import           Network.Wai                               (Middleware)
import           Network.Wai.Handler.Warp                  (Settings,
                                                            defaultSettings,
                                                            setGracefulShutdownTimeout,
                                                            setInstallShutdownHandler,
                                                            setPort)
import           Network.Wai.Handler.Warp                  as Warp
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON
import           Register
import           Relude
import           Servant
import           Servant.Auth.Server
import           Server
import           Server.Protected
import           Server.Public
import           System.Environment                        (lookupEnv)
import           System.Log.FastLogger
import           System.Posix.Signals                      (Handler (..),
                                                            installHandler,
                                                            sigINT, sigTERM)
import           User
import Servant (Server)
import App
import Config
import Cryptography
import Control.Monad.Cont

run :: Config -> IO ()
run Config{..} = do

  jwk <- readJWK cfgKeyPath

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
      dbBracket = ContT $ bracket
        (openLocalStateFrom cfgAcidPath (Database mempty))
        (\db -> closeAcidState db >> print "Acid State closed")

      finalAction db = do

        
        let env = Env db appLogger

         -- Servant context assembly
        let jwts = defaultJWTSettings jwk
            cs = defaultCookieSettings {cookieIsSecure = NotSecure}
            authCfg = authCheck db :: BasicAuthCfg
            cfg = cs :. jwts :. authCfg :. EmptyContext
            cfgProxy = Proxy :: Proxy '[CookieSettings, JWTSettings, BasicAuthCfg]
            env = Env db appLogger jwts cs 
            server = hoistServerWithContext api cfgProxy (nt env) serverT

            -- Application
            app = serveWithContext api cfg server 

        runSettings settings . warpLogger $ app

        
  runContT dbBracket finalAction

  exitSuccess


-- | Construct json logger
jsonRequestLogger :: IO Middleware
jsonRequestLogger =
  mkRequestLogger $ def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}


-- | Construct simple stdout logger
stdOutLogger :: IO Middleware
stdOutLogger = return logStdout


-- | Make Wai Settings that handles app termination signals with provided shutdownAction
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
