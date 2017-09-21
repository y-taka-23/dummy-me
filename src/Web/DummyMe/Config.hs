module Web.DummyMe.Config (
      Config(..)
    , getConfig
    ) where

import Options.Applicative

data Config = Config {
      file      :: FilePath
    , ident     :: String
    , port      :: Int
    , quiet     :: Bool
    , snapshots :: FilePath
    , version   :: Bool
    }

getConfig :: IO Config
getConfig = customExecParser (prefs showHelpOnError) configParserInfo

fileP :: Parser String
fileP = option str $ mconcat [
      short 'f'
    , long "file"
    , value "db.json"
    , help "JSON file which prescribes the initial data"
    , metavar "string"
    , showDefault
    ]

identP :: Parser String
identP = option str $ mconcat [
      long "id"
    , value "id"
    , help "Identifier for records in the database"
    , metavar "string"
    , showDefault
    ]

portP :: Parser Int
portP = option auto $ mconcat [
      short 'p'
    , long "port"
    , value 8080
    , help "Port for the REST endpoints"
    , metavar "int"
    , showDefault
    ]

quietP :: Parser Bool
quietP = switch $ mconcat [
      long "quiet"
    , help "Suppress log messages"
    ]

snapshotsP :: Parser String
snapshotsP = option str $ mconcat [
      long "snapshots"
    , value "."
    , help "Directory to store the database snapshots"
    , metavar "string"
    , showDefault
    ]

-- TODO: For which -v should be --version or --verbose?
versionP :: Parser Bool
versionP = switch $ mconcat [
      long "version"
    , help "Show the version information"
    ]

configP :: Parser Config
configP = helper <*> (
        Config
    <$> fileP
    <*> identP
    <*> portP
    <*> quietP
    <*> snapshotsP
    <*> versionP
    )

configParserInfo :: ParserInfo Config
configParserInfo = info configP $ mconcat [
      progDesc "A simple dummy server for REST APIs"
    ]
