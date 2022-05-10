{-# LANGUAGE TemplateHaskell #-}

module DB where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ask)
import           Control.Monad.State    (get, put)
import           Data.Acid
import           Data.Generics.Labels
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Password.Bcrypt
import           Data.SafeCopy
import           Data.Text              (Text)
import           Data.Typeable
import           Data.UUID
import           Data.UUID.V4           (nextRandom)
import           Register               (Register)
import qualified Register
import           Relude
import           Types                  (Email)
import           User

newtype Database = Database
  { users :: Map UUID User
  }
  deriving stock (Generic, Show, Typeable)

$(deriveSafeCopy 0 'base ''Database)

getUserByEmailM :: MonadReader Database m => Email -> m (Either Text User)
getUserByEmailM email = do
  m <- asks users
  let mbFoundUser = head <$>
        ( nonEmpty
          . Map.elems
          . Map.filter (\u -> email == uEmail u) $ m)
  case mbFoundUser of
    Nothing        -> return . Left $ "User not found with such Email:" <> email
    Just foundUser -> return . Right $ foundUser


-- API

registerUser :: User -> Update Database (Either Text User)
registerUser user = do
  (Database m) <- get
  eitherFoundUser <- gets (getUserByEmailM (user ^. #uEmail))
  case eitherFoundUser of
    Right _ -> return $ Left "User already present"
    Left _ -> do
      #users . at (user ^. #uId) .= Just user
      return $ Right user


check :: Text -> User -> Either Text User
check p storedUser =
  let ph = PasswordHash . uPasswordHash $ storedUser :: PasswordHash Bcrypt
   in case checkPassword (mkPassword p) ph of
        PasswordCheckSuccess -> Right storedUser
        PasswordCheckFail    -> Left "Wrong password"


getAllUsers :: Query Database [User]
getAllUsers = do
  Database m <- ask
  return . fmap snd . Map.toList $ m


getUser :: Email -> Text -> Query Database (Either Text User)
getUser email p = do
  Database m <- ask
  eitherFoundUser <- getUserByEmailM email
  let p' = mkPassword p
      u = eitherFoundUser >>= check p
  return u


getUserByEmail :: Email -> Query Database (Either Text User)
getUserByEmail = getUserByEmailM


deleteUser :: Email -> Update Database (Either Text ())
deleteUser email = do
  (Database m) <- get
  eitherFoundUser <- gets (getUserByEmailM email)
  case eitherFoundUser of
    Left e -> return . Left $ e
    Right foundUser -> do
      #users . at (foundUser ^. #uId) .= Nothing
      return $ Right ()

$(makeAcidic ''Database ['registerUser, 'getUser, 'getUserByEmail, 'getAllUsers, 'deleteUser])
