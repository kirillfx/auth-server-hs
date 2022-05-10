module Server.Public where

import           API.Public
import           Env
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           DB
import           Data.Acid
import           Data.Password.Bcrypt
import           Data.Text              (Text)
import           Data.Time.Clock        (getCurrentTime)
import           Data.UUID.V4           (nextRandom)
import           Logging
import           Register
import           Relude
import           Servant                hiding (BasicAuth)
import           Servant.Auth.Server
import           Servant.Client
import           System.Log.FastLogger
import           User
import App

-- User registration handler
registerH :: Register -> App ()
registerH r@(Register e p) = do
  liftIO . print $ "Register" <> show r
  i <- liftIO nextRandom
  p' <- liftIO $ hashPassword (mkPassword p)
  let newUser = User i e (unPasswordHash p')
  Env{..} <- ask
  eitherUser <- liftIO $ update database (RegisterUser newUser)
  case eitherUser of
    Left e -> throwError $ UnexpectedError "Can't register"
    Right u -> do
      tstamp <- liftIO getCurrentTime
      let logMsg =
            LogMessage
              { message = e <> " registered",
                timestamp = tstamp,
                level = "info"
              }
      liftIO $ pushLogStrLn getLogger $ toLogStr logMsg
      return ()

indexH :: App Text
indexH = return "Index"

publicServerT :: ServerT PublicAPI App
publicServerT = registerH :<|> indexH
