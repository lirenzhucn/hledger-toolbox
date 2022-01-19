module Params where

import Hledger.Utils.String (strip)
import Options.Applicative

data Params = Params
  { outputFile :: FilePath,
    noPullData :: Bool,
    settingsFile :: FilePath,
    dbDir :: FilePath
  }

mkParams :: Parser Params
mkParams =
  Params
    <$> strArgument (metavar "OUTPUT" <> help "Output file path")
    <*> switch
      ( long "no-pull-data" <> short 'n'
          <> help "Do not pull data from API"
      )
    <*> option
      auto
      ( long "settings" <> short 's'
          <> help "Path to the settings file"
          <> showDefault
          <> value "./.secrets/ynab_api_import_settings.json"
          <> metavar "SETTINGS"
      )
    <*> option
      auto
      ( long "db-dir" <> short 'd'
          <> help "Path to the base dir of local databases"
          <> showDefault
          <> value "./.secrets/dbs"
          <> metavar "DBS_DIR"
      )

cliParser :: IO Params
cliParser = execParser opts
  where
    opts =
      info
        (mkParams <**> helper)
        (fullDesc <> progDesc "YNAB data adapter for hledger")
