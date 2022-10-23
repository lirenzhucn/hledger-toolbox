{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Config where

import Control.Exception.Safe ( try )
import Data.Aeson ( (.:?), FromJSON(..), withObject, withText )
import Options.Applicative
import Data.List ( intersperse )
import qualified Data.Text as T
import Data.Maybe ( fromMaybe )
import Data.Yaml ( prettyPrintParseException )
import Data.Yaml.Config ( ignoreEnv, loadYamlSettings )
import System.Directory ( doesFileExist )
import System.Environment ( lookupEnv )
import System.Environment.XDG.BaseDir ( getUserConfigFile )
import System.Exit ( exitFailure )

import StockPrices.Types ( PriceDataFrequency (..) )
import Utils ( logError, logWarning )

instance FromJSON PriceDataFrequency where
  parseJSON = withText "PriceDataFrequency" (return . read . T.unpack)

data AppConfig = AppConfig
  { apiKey              :: String
  , rateLimit           :: Bool
  , journalFile         :: FilePath
  , outputFile          :: FilePath
  , excludedCommodities :: [String]
  , cryptoCommodities   :: [T.Text]
  , frequency           :: PriceDataFrequency
  , dryRun              :: Bool
  }
  deriving (Show, Eq)

data ConfigFile = ConfigFile
  { cfgApiKey              :: Maybe String
  , cfgRateLimit           :: Maybe Bool
  , cfgExcludedCommodities :: Maybe [String]
  , cfgCryptoCommodities   :: Maybe [String]
  , cfgFrequency           :: Maybe PriceDataFrequency
  }
  deriving (Show, Eq)

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \o -> do
    cfgApiKey <- o .:? "api-key"
    cfgRateLimit <- o .:? "rate-limit"
    cfgExcludedCommodities <- o .:? "exclude"
    cfgCryptoCommodities <- o .:? "crypto"
    cfgFrequency <- o .:? "frequency"
    return ConfigFile { .. }

data Args = Args
  { argApiKey              :: Maybe String
  , argNoRateLimit         :: Bool
  , argJournalFile         :: Maybe FilePath
  , argOutputFile          :: FilePath
  , argExcludedCommodities :: [String]
  , argCryptoCommodities   :: [String]
  , argFrequency           :: Maybe PriceDataFrequency
  , argDryRun              :: Bool
  }
  deriving (Show, Eq)

parseStringList :: Monad m => String -> m [String]
parseStringList = return . words

multiString desc = concat <$> many single
  where single = option (str >>= parseStringList) desc

cliParser :: IO Args
cliParser = execParser opts
  where
    opts = info (mkArgs <**> helper) (fullDesc <> progDesc "Pull the prices of stocks in the journal")
    -- make the Args object from cli input
    mkArgs :: Parser Args
    mkArgs =
      Args
        <$>
          ( optional $ strOption $ long "api-key" <> short 'a'
          <> help "The AlphaVantage API key" <> metavar "API_KEY"
          )
        <*> switch
          ( long "no-rate-limit" <> short 'n'
          <> help "Do not impose rate limit"
          )
        <*>
          ( optional $ strOption $ long "journal-file" <> short 'j'
          <> help "The journal file from which the commodities can be extracted"
          <> metavar "JOURNAL_FILE"
          )
        <*> strOption
          ( long "output-file" <> short 'o'
          <> showDefault
          <> value "prices.journal"
          <> help "The output file for prices"
          <> metavar "OUTPUT_FILE"
          )
        <*> multiString
          ( long "exclude" <> short 'e'
          <> help "List of excluded commodities"
          <> metavar "EXCLUDE"
          )
        <*> multiString
          ( long "crypto" <> short 'c'
          <> help "List of cryptocurrency commodities"
          <> metavar "CRYPTO"
          )
        <*>
          ( optional $ option auto $ long "frequency" <> short 'f'
          <> help "Frequency of prices; valid options are daily, weekly, and monthly"
          <> metavar "FREQUENCY"
          )
        <*> switch
          ( long "dry-run" <> short 'd'
          <> help "Display the commodities and dates without actually querying prices"
          )

-- Find and load the config file (config.yaml)
loadConfigFile :: IO ConfigFile
loadConfigFile = do
  configFile <- getUserConfigFile "stockprices" "config.yaml"
  hasConfig <- doesFileExist configFile
  if hasConfig
    then try (loadYamlSettings [configFile] [] ignoreEnv) >>= \case
      Left (lines . prettyPrintParseException -> errorMsg) ->
        logWarning (strJoin "\t" ("Invalid configuration file format:" : errorMsg))
        >> return defaultConfig
      Right c -> return c
    else return defaultConfig
  where
    defaultConfig :: ConfigFile
    defaultConfig = ConfigFile Nothing Nothing Nothing Nothing Nothing
    strJoin :: String -> [String] -> String
    strJoin sep strList = foldl (++) "" $ intersperse sep strList

-- Merge the arguments, env vars, and values found in the config file into an
-- 'AppConfig.
mergeArgsEnvCfg :: ConfigFile -> Args -> IO AppConfig
mergeArgsEnvCfg ConfigFile {..} Args {..} = do
  envJournalFile <- lookupEnv "LEDGER_FILE"
  envApiKey <- lookupEnv "ALPHAVANTAGE_KEY"
  apiKey <- case argApiKey <|> envApiKey <|> cfgApiKey of
    Just k -> return k
    Nothing ->
      logError "Pass an AlphaVantage API key with `-a` or $ALPHAVANTAGE_KEY"
      >> exitFailure
  let journalFile =
        fromMaybe "~/.hledger.journal" $ argJournalFile <|> envJournalFile
      rateLimit = not argNoRateLimit
      excludedCommodities =
        if null argExcludedCommodities
          then fromMaybe defaultExcludedCommodities cfgExcludedCommodities
          else defaultExcludedCommodities
      cryptoCommodities =
        if null argCryptoCommodities
          then maybe [] (map T.pack) cfgCryptoCommodities
          else concatMap (T.splitOn "," . T.pack) argCryptoCommodities
      outputFile = argOutputFile
      frequency  = fromMaybe Weekly $ argFrequency <|> cfgFrequency
      dryRun     = argDryRun
  return AppConfig {..}
  where
    defaultExcludedCommodities :: [String]
    defaultExcludedCommodities = ["$", "AUTO", "USD"]
