module CliOptions where

import           Data.Int            (Int32)
import qualified Data.Int            as DI
import           Data.Text
import           Options.Applicative
import           Relude

data Options
  = RunService FilePath
  | GenerateJWK FilePath
  | GenerateToken (FilePath, Text)

-- | Parses 'Options' from command line arguments.
parseOptions :: IO Options
parseOptions = execParser opts

-- | Constructs output for help description.
opts :: ParserInfo Options
opts =
  info (cliArgs <**> helper) $
    fullDesc
      <> progDesc
        "Run executable in service, jwk generation, token generation modes."
      <> header "auth-service"

-- | Parses command line options.
cliArgs :: Parser Options
cliArgs =
  subparser
    ( command "generateJWK" (info (GenerateJWK <$> genJwkParser) generateJWKDesc)
        <> command "generateToken" (info (GenerateToken <$> genTokenParser) generateTokenDesc)
    )
    <|> RunService <$> configParser
  where
    generateJWKDesc = progDesc "Generates JWK"
    generateTokenDesc = progDesc "Generates Access token with provided jwk and userId"

-- | Parses the complete service 'Config'.
configParser :: Parser FilePath
configParser =
  argument str $
    help "Run service with provided config"
      <> metavar "CONFIGPATH"

-- | Parses '(FilePath, Text)' to use in token generation
genTokenParser :: Parser (FilePath, Text)
genTokenParser = (,) <$> jwkParser <*> coreUserIdParser
  where
    jwkParser =
      argument str $
        metavar "JWKPATH"
    coreUserIdParser =
      argument str $
        metavar "COREUSERID"

-- | Parses 'FilePath' to where the JWK key for generating JWT is stored.
genJwkParser :: Parser FilePath
genJwkParser =
  argument str $
    help "Output location where to save the generated JSON Web Key"
      <> metavar "JWKPATH"
