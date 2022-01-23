module Params where

import Options.Applicative

data Params = Params
  { inputFile :: FilePath,
    outputFile :: FilePath,
    settingsFile :: FilePath
  }

mkParams :: Parser Params
mkParams =
  Params
    <$> strArgument (metavar "INPUT" <> help "Input path to the paycheck")
    <*> strArgument (metavar "OUTPUT" <> help "Output file path")
    <*> strOption
      ( long "settings" <> short 's'
          <> help "Path to the settings file"
          <> showDefault
          <> value "./.secrets/paycheck_import_settings.json"
          <> metavar "SETTINGS"
      )

cliParser :: IO Params
cliParser = execParser opts
  where
    opts =
      info
        (mkParams <**> helper)
        (fullDesc <> progDesc "Import paychecks to hledger")
