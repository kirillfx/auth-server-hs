{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DB
  ( insertUser,
    getUsers,
    getUser,
    removeUser,
    Database (..),
  )
where

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
import User

data Database = Database (Map UUID User)
  deriving (Typeable)

$(deriveSafeCopy 0 'base ''Database)

-- API

insertUser :: User -> Update Database (Either String User)
insertUser u = undefined

check :: Text -> User -> Either String User
check p u =
  let ph = PasswordHash . passwordHash $ u :: PasswordHash Bcrypt
   in case checkPassword (mkPassword p) ph of
        PasswordCheckSuccess -> Right u
        PasswordCheckFail -> Left "Wrong password"

getUsers :: Query Database [User]
getUsers = do
  Database m <- ask
  return . fmap snd . Map.toList $ m

getUser :: Text -> Text -> Query Database (Either String User)
getUser l p = do
  Database m <- ask
  let p' = mkPassword p
      u = (safeHead . fmap snd . Map.toList . Map.filter (\u -> username u == l) $ m) >>= (check p)
  return u
  where
    safeHead [] = Left "Not found"
    safeHead (x : _) = Right x

removeUser :: UUID -> Update Database (Either String ())
removeUser key = do
  (Database m) <- get
  put (Database (Map.delete key m))
  return $ Right ()

$(makeAcidic ''Database ['insertUser, 'getUser, 'removeUser])

-- dbInsertPerson :: Person -> Update PersonDB ()
-- dbInsertPerson person = do
--   let key = personId person
--   PersonDB m <- S.get
--   S.put (PersonDB (Map.insert key person m))

-- dbGetPerson :: Key -> Query PersonDB (Maybe Person)
-- dbGetPerson key = do
--   PersonDB m <- ask
--   return (Map.lookup key m)