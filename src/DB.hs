{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DB where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Password.Bcrypt
import Data.SafeCopy
import Data.Text (Text)
import Data.Typeable
import Data.UUID
import Data.UUID.V4 (nextRandom)
import Register (Register)
import qualified Register
import User

data Database = Database (Map UUID User)
  deriving (Typeable)

$(deriveSafeCopy 0 'base ''Database)

-- API

registerUser :: User -> Update Database (Either String User)
registerUser user = do
  (Database m) <- get
  case (safeHead . fmap snd . Map.toList . Map.filter (\u -> email user == email u) $ m) of
    Left e -> do
      put (Database (Map.insert (userId user) user m))
      return $ Right user
    Right user ->
      return $ Left "User already present"

check :: Text -> User -> Either String User
check p u =
  let ph = PasswordHash . passwordHash $ u :: PasswordHash Bcrypt
   in case checkPassword (mkPassword p) ph of
        PasswordCheckSuccess -> Right u
        PasswordCheckFail -> Left "Wrong password"

getAllUsers :: Query Database [User]
getAllUsers = do
  Database m <- ask
  return . fmap snd . Map.toList $ m

safeHead :: [b] -> Either String b
safeHead [] = Left "Not found"
safeHead (x : _) = Right x

getUser :: Text -> Text -> Query Database (Either String User)
getUser e p = do
  Database m <- ask
  let p' = mkPassword p
      u = (safeHead . fmap snd . Map.toList . Map.filter (\u -> email u == e) $ m) >>= (check p)
  return u

getUserByEmail :: Text -> Query Database (Either String User)
getUserByEmail e = do
  Database m <- ask
  let u = safeHead . fmap snd . Map.toList . Map.filter (\u -> email u == e) $ m
  return u

deleteUser :: Text -> Update Database (Either String ())
deleteUser e = do
  (Database m) <- get
  case safeHead . fmap snd . Map.toList . Map.filter (\u -> email u == e) $ m of
    Left e -> return . Left $ e
    Right u -> do
      put (Database (Map.delete (userId u) m))
      return $ Right ()

$(makeAcidic ''Database ['registerUser, 'getUser, 'getUserByEmail, 'getAllUsers, 'deleteUser])