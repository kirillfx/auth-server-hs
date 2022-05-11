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
import Servant.Server.Generic
import Servant.API.Generic
import Network.HTTP.Client (setQueryStringPartialEscape, httpNoBody, setQueryString)
import qualified Data.UUID as UUID
import Env
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Except (liftEither)
import AuthToken
import Env (Env(jwtSettings))

publicServerT :: ToServant PublicRoutes (AsServerT App)
publicServerT =
  genericServerT $
    PublicRoutes
      { _register = registerH
      , _index = indexH
      }


-- User registration handler
registerH :: Register -> App (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Text)
registerH r@(Register e p) = do
  liftIO . print $ "Register" <> show r
  i <- liftIO nextRandom
  p' <- liftIO $ hashPassword (mkPassword p)
  let newUser = User i e (unPasswordHash p')
      authToken = fromUser newUser
  Env{..} <- ask
  user <- liftIO (update database (RegisterUser newUser)) >>= liftEither . first UnexpectedError
  token <- liftIO (makeJWT authToken jwtSettings Nothing) >>= liftEither . first (UnexpectedError . show)
  mApplyCookies <- liftIO $ acceptLogin csSettings jwtSettings authToken
  case mApplyCookies of
    Nothing -> throwError $ NotAuthorized "Can't apply cookies"
    Just applyCookies -> do
      tstamp <- liftIO getCurrentTime
      let logMsg =
            LogMessage
              { message = e <> " registered",
                timestamp = tstamp,
                level = "info"
              }

      -- call webhooks
      let qsApply = setQueryString [("userId", Just . LBS.toStrict . UUID.toByteString $ i)]
      liftIO $ mapM_ (\r -> httpNoBody (qsApply r) manager) webhooks
      liftIO $ pushLogStrLn getLogger $ toLogStr logMsg

      return $ applyCookies . decodeUtf8 $ token


-- TODO: Does nothing and can be deleted
indexH :: App Text
indexH = return "Index"

