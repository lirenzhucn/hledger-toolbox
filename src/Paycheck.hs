module Paycheck where

import Control.Monad.Logger (runStderrLoggingT, logInfoN, logWarnN)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import Data.Text.Lazy (toStrict)
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Hledger (BalancingOpts (..), Journal)
import Hledger.Data.Balancing (journalBalanceTransactions)
import System.FilePath.Glob (compile, globDir)
import TextShow (showtList)

import Hledger.Utils.Render (renderJournal)
import Paycheck.Hledger (makeJournal)
import Paycheck.Types (AppSettings, PaycheckApp (runApp))
import Utils (readPdfOrTxt_)

runPaycheckApp :: PaycheckApp a -> AppSettings -> IO a
runPaycheckApp app settings = runStderrLoggingT (runReaderT (runApp app) settings)

parsePaychecksApp :: FilePath -> PaycheckApp Journal
parsePaychecksApp inputDir = do
  settings <- ask
  currTime <- liftIO getPOSIXTime
  inputFiles <- liftIO $ fmap concat (globDir (map compile ["*.pdf", "*.txt"]) inputDir)
  logFilesFound inputFiles
  contents <- liftIO $ mapM readPdfOrTxt_ inputFiles
  pure $ makeJournal currTime settings contents
  where
    logFilesFound :: [FilePath] -> PaycheckApp [FilePath]
    logFilesFound fs = do
      logInfoN $ "Found files to process: " <> showtList fs
      pure fs

printJournalApp :: Journal -> PaycheckApp ()
printJournalApp = liftIO . TIO.putStrLn . toStrict . renderJournal

balanceOrWarnApp :: Journal -> PaycheckApp Journal
balanceOrWarnApp journal = do
  let opts = BalancingOpts True False Nothing
      eitherJournal = journalBalanceTransactions opts journal
  case eitherJournal of
    Left m -> do
      logWarnN "Unable to balance the resulting journal"
      pure journal
    Right j -> do
      logInfoN "Journal balanced!"
      pure j
