module Params where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

data Params = Params
  { outputFile    :: FilePath
  , year          :: String
  , settingsFile  :: FilePath
  , cliServerUrl  :: Maybe Text
  , cliPassword   :: Maybe Text
  , cliSyncId     :: Maybe Text
  , cliEncPwd     :: Maybe Text
  , cliCaCert     :: Maybe FilePath
  }

mkParams :: Parser Params
mkParams =
  Params
    <$> strArgument
      ( metavar "OUTPUT"
          <> help "Output file path; use - for stdout"
      )
    <*> strOption
      ( long "year"
          <> short 'y'
          <> help "Year to import (YYYY), or 'all'"
          <> showDefault
          <> value "all"
          <> metavar "YEAR"
      )
    <*> strOption
      ( long "settings"
          <> short 's'
          <> help "Path to the settings JSON file"
          <> showDefault
          <> value "./.secrets/actual_import_settings.json"
          <> metavar "SETTINGS"
      )
    <*> optional (T.pack <$> strOption
      ( long "server-url"
          <> help "Actual server URL (overrides ACTUAL_SERVER_URL env var and settings file)"
          <> metavar "URL"
      ))
    <*> optional (T.pack <$> strOption
      ( long "password"
          <> help "Actual server password (overrides ACTUAL_PASSWORD env var and settings file)"
          <> metavar "PWD"
      ))
    <*> optional (T.pack <$> strOption
      ( long "sync-id"
          <> help "Actual budget sync ID (overrides ACTUAL_SYNC_ID env var and settings file)"
          <> metavar "SYNC_ID"
      ))
    <*> optional (T.pack <$> strOption
      ( long "encryption-password"
          <> help "Actual E2E encryption password (overrides ACTUAL_ENCRYPTION_PASSWORD env var)"
          <> metavar "ENC_PWD"
      ))
    <*> optional (strOption
      ( long "ca-cert"
          <> help "Path to CA certificate for self-signed TLS (overrides ACTUAL_CA_CERT env var)"
          <> metavar "CERT"
      ))

cliParser :: IO Params
cliParser = execParser opts
  where
    opts =
      info
        (mkParams <**> helper)
        (fullDesc <> progDesc "Actual Budget data adapter for hledger")
