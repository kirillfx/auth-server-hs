module Main where

import           CliOptions
import           Control.Monad.IO.Class
import           Cryptography
import           Data.Default           (def)
import qualified Data.Yaml              as YAML
import           Relude
import           Servant.Auth.Server    (defaultJWTSettings, makeJWT)
import qualified Server
import           AuthToken
import Config
import Application (run)
import qualified Data.UUID as UUID

main :: IO ()
main =
  parseOptions >>= \case
    GenerateToken (path, userIdText) -> do
      jwts <- defaultJWTSettings <$> readJWK path

      userId <- do
        case UUID.fromText userIdText of
          Nothing -> putStrLn "Can't decode UserId" >> exitFailure
          Just userId -> pure userId
          
      let token :: AuthToken
          token = AuthToken userId
      
      et <- makeJWT token jwts Nothing
      case et of
        Left e  -> print e >> exitFailure
        Right t -> print t >> exitSuccess
    GenerateJWK path -> generateKeyPairIO path
    RunService path ->
      YAML.decodeFileThrow @_ @Config path >>= run
