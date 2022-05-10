module Server.Protected where

import           API.Protected
import           Env
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           DB
import           Data.Acid
import           Data.ByteString
import qualified Data.ByteString        as BS
import           Data.ByteString.Lazy   as BL
import           Data.Password.Bcrypt
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time.Clock        (getCurrentTime)
import           Data.UUID.V4           (nextRandom)
import           Logging
import           Login
import           Register
import           Relude
import           Servant                hiding (BasicAuth)
import           Servant.Auth.Server
import           Servant.Client
import AuthToken
import           System.Log.FastLogger
import           User
import App
import qualified Data.UUID as UUID
import Control.Lens
import Data.Generics.Labels
import Env (Env(csSettings, jwtSettings))


basicAuthProtectedServer :: AuthResult AuthToken -> ServerT BasicAuthProtectedAPI App
basicAuthProtectedServer = loginH


loginH :: AuthResult AuthToken -> App (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthToken)
loginH authResult = do
  authToken <- fromAuthResult authResult
  cs <- asks csSettings
  jwts <- asks jwtSettings
  mApplyCookies <- liftIO $ acceptLogin cs jwts authToken
  case mApplyCookies of
    Nothing -> throwError $ NotAuthorized "Can't apply cookies"
    Just applyCookies -> do
      etoken <- liftIO $ makeJWT authToken jwts Nothing
      case etoken of
        Left e -> throwError $ NotAuthorized . show $ e
        Right token -> do
          liftIO $ print token
          return $ applyCookies authToken


jwtProtectedServerT :: AuthResult AuthToken -> ServerT JWTProtectedAPI App
jwtProtectedServerT authResult = userDetailsH authResult :<|> deleteUserH authResult :<|> authH


userDetailsH :: AuthResult AuthToken -> App User
userDetailsH authResult = do
  authToken <- fromAuthResult authResult
  Env{..} <- ask
  eitherUser <- liftIO $ query database (GetUserByUserId $ authToken ^. #userId)
  case eitherUser of
    Left e  -> throwError $ UnexpectedError . show $ e
    Right u -> return u


userIdFromText :: Text -> App UUID.UUID
userIdFromText x = case UUID.fromText x of
  Nothing -> throwError $ UnexpectedError ("Can't decode UUID from: " <> x)
  Just uuid -> pure uuid


deleteUserH :: AuthResult AuthToken -> App ()
deleteUserH authResult = do
  userId <- fromAuthResult authResult <&> userId 
  Env{..} <- ask
  eitherDelete <- liftIO $ update database (DeleteUser userId)
  case eitherDelete of
    Left e -> throwError $ UnexpectedError e
    Right u -> do
      tstamp <- liftIO getCurrentTime
      let logMsg =
            LogMessage
              { message = UUID.toText userId <> " deleted",
                timestamp = tstamp,
                level = "info"
              }
      liftIO $ pushLogStrLn getLogger $ toLogStr logMsg
      return ()


authH :: App NoContent
authH = return NoContent

protectedServerT = basicAuthProtectedServer :<|> jwtProtectedServerT

-- Basic auth check function for working with AcidState Database
authCheck :: AcidState Database -> BasicAuthData -> IO (AuthResult AuthToken)
authCheck database (BasicAuthData ebs pbs) = do
  let e = decodeUtf8 ebs
      p = decodeUtf8 pbs
  eitherUser <- liftIO $ query database (GetUser e p)
  case eitherUser of
    Left e  -> return Indefinite
    Right u -> return (Authenticated (AuthToken.fromUser u))
