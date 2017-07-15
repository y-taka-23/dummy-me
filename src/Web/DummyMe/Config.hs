module Web.DummyMe.Config (
      Config(..)
    , getConfig
    ) where

import Options.Applicative

data Config = Config {
      file :: FilePath
    , port :: Int
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

portP :: Parser Int
portP = option auto $ mconcat [
      short 'p'
    , long "port"
    , value 8080
    , help "Port for the REST endpoints"
    , metavar "int"
    , showDefault
    ]

configP :: Parser Config
configP = helper <*> (Config <$> fileP <*> portP)

configParserInfo :: ParserInfo Config
configParserInfo = info configP $ mconcat [
      progDesc "A simple dummy server for REST APIs"
    ]
