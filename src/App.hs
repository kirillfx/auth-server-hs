module App where

import Relude
import Control.Monad.Reader
import Servant.Server
import Servant.Auth.Server (AuthResult(..))
import Env
import Control.Monad.Catch hiding (Handler)
import Control.Monad
import Control.Monad.Except
import qualified Data.Text as T 


data AuthServiceError
  = NotAuthorized Text
  | UnexpectedError Text
  deriving stock (Generic, Show, Eq)


newtype App a = App
  { runApp :: ReaderT Env (ExceptT AuthServiceError IO) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadMask,
      MonadCatch,
      MonadThrow,
      MonadReader Env,
      MonadIO,
      MonadError AuthServiceError
    )

-- | Transform 'App' -> 'Handler'
nt :: Env -> App a -> Handler a
nt e x = Handler . withExceptT fromAuthServiceError . runReaderT (runApp x) $ e


fromAuthResult :: Show a => AuthResult a -> App a
fromAuthResult (Authenticated uid) = return uid
fromAuthResult x                   = throwError . NotAuthorized
                                   . T.pack
                                   . show $ x

fromAuthServiceError :: AuthServiceError -> ServerError
fromAuthServiceError = \case
  NotAuthorized t -> err401 { errBody = encodeUtf8 t}
  UnexpectedError t -> err500 {errBody = encodeUtf8 t}
