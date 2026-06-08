module Main where

import Actual.Cli (fetchTransactions)
import Actual.Hledger (buildConfig, makeJournal)
import Actual.Types
import Control.Monad (unless)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Hledger.Utils.Render (renderJournal)
import Params
import System.Environment (lookupEnv)
import System.Exit (die)
import Utils (decodeFromJSON)

main :: IO ()
main = do
  params  <- cliParser
  content <- TIO.readFile (settingsFile params)
  case (decodeFromJSON content :: Maybe AppSettingsJSON) of
    Nothing   -> die "Failed to parse settings file"
    Just json -> do
      settings <- resolveSettings json params
      work params settings

resolveSettings :: AppSettingsJSON -> Params -> IO AppSettings
resolveSettings json params = do
  envUrl    <- lookupEnv "ACTUAL_SERVER_URL"
  envPwd    <- lookupEnv "ACTUAL_PASSWORD"
  envSyncId <- lookupEnv "ACTUAL_SYNC_ID"
  envEncPwd <- lookupEnv "ACTUAL_ENCRYPTION_PASSWORD"
  envCaCert <- lookupEnv "ACTUAL_CA_CERT"
  let pas = asjActualSettings json
      pick cli env jsonVal = cli <|> (T.pack <$> env) <|> jsonVal
      serverUrl = pick (cliServerUrl params) envUrl    (pasServerUrl pas)
      password  = pick (cliPassword  params) envPwd    (pasPassword  pas)
      syncId    = pick (cliSyncId    params) envSyncId (pasSyncId    pas)
      encPwd    = pick (cliEncPwd    params) envEncPwd (pasEncryptionPassword pas)
      caCert    = cliCaCert params <|> envCaCert <|> asjCaCert json
      missing   = [n | (n, Nothing) <- [ ("server_url", serverUrl)
                                        , ("password",   password)
                                        , ("sync_id",    syncId) ]]
  unless (null missing) $
    die $ "Missing required credentials (set via --flag, env var, or settings file): "
          ++ intercalate ", " missing
  return AppSettings
    { actual_settings = ActualSettings
        { server_url          = fromJust serverUrl
        , password            = fromJust password
        , sync_id             = fromJust syncId
        , encryption_password = encPwd
        }
    , data_dir                 = asjDataDir json
    , ca_cert                  = caCert
    , starting_balance_account = asjStartingBalanceAccount json
    , transfer_account         = asjTransferAccount json
    , account_map              = asjAccountMap json
    , payee_map                = asjPayeeMap json
    }

(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> r = r
l       <|> _ = l

work :: Params -> AppSettings -> IO ()
work params settings = do
  let yearFilter = case year params of
        "all" -> Nothing
        y     -> Just y
  transactions <- fetchTransactions settings yearFilter
  currTime     <- getPOSIXTime
  let config  = buildConfig settings currTime
      journal = makeJournal config transactions
      output  = renderJournal journal
  case outputFile params of
    "-" -> LTIO.putStr output
    f   -> LTIO.writeFile f output
