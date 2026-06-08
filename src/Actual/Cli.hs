module Actual.Cli (fetchTransactions) where

import Actual.Db (fetchAllFromDb)
import Actual.Types
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

-- | Fetch all transactions from Actual Budget via the `actual` CLI.
-- Resolves account/payee/category UUIDs to display names.
-- If yearStr is Nothing or "all", fetches all available transactions.
fetchTransactions :: AppSettings -> Maybe String -> IO [Transaction]
fetchTransactions settings yearStr = do
  -- One CLI call to sync the local database; result is discarded.
  _ <- (callCli settings ["accounts", "list", "--include-closed"] :: IO [RawAccount])
  (accounts, payees, groups, rawTxs) <-
    fetchAllFromDb (data_dir settings) (sync_id (actual_settings settings)) (dateRangeInt yearStr)
  let accById = M.fromList [(raId a, raName a) | a <- accounts]
      payById = M.fromList [(rpId p, rpName p) | p <- payees]
      catById = M.fromList
        [ (rcId c, (rcName c, rcgName g, rcIsIncome c || rcgIsIncome g))
        | g <- groups, c <- rcgCategories g ]
  return $ concatMap (flatten accById payById catById) rawTxs

-- | Flatten a raw transaction into enriched transactions.
-- Split parents are replaced by their subtransactions (each gets parent's
-- account/date/payee/cleared if not already set on the child).
flatten
  :: M.Map Text Text                    -- account UUID → name
  -> M.Map Text Text                    -- payee UUID → name
  -> M.Map Text (Text, Text, Bool)      -- category UUID → (name, group, isIncome)
  -> RawTx
  -> [Transaction]
flatten accById payById catById tx
  | rtIsParent tx = map (enrich accById payById catById . fillFromParent tx)
                        (rtSubtransactions tx)
  | otherwise     = [enrich accById payById catById tx]

-- Give a child its parent's account/date/cleared when they are absent
-- (in practice the DB always populates these on children, but be defensive).
fillFromParent :: RawTx -> RawTx -> RawTx
fillFromParent parent child = child
  { rtAccount = rtAccount parent
  , rtDate    = rtDate parent
  , rtPayee   = if rtPayee child /= Nothing then rtPayee child else rtPayee parent
  , rtCleared = rtCleared parent
  }

enrich
  :: M.Map Text Text
  -> M.Map Text Text
  -> M.Map Text (Text, Text, Bool)
  -> RawTx
  -> Transaction
enrich accById payById catById tx =
  Transaction
    { txId              = rtId tx
    , txAccountName     = fromMaybe (rtAccount tx) (M.lookup (rtAccount tx) accById)
    , txDate            = rtDate tx
    , txAmount          = rtAmount tx
    , txPayeeName       = resolvePayee tx
    , txCategoryName    = fmap (\(n,_,_) -> n) catInfo
    , txCategoryGroup   = fmap (\(_,g,_) -> g) catInfo
    , txIsIncomeCat     = maybe False (\(_,_,i) -> i) catInfo
    , txNotes           = rtNotes tx
    , txCleared         = rtCleared tx
    , txTransferId      = rtTransferId tx
    , txStartingBalance = rtStartingBalanceFlag tx
    }
  where
    catInfo = rtCategory tx >>= \cid -> M.lookup cid catById
    resolvePayee t =
      case rtPayee t of
        Nothing  -> rtImportedPayee t
        Just pid -> case M.lookup pid payById of
          Just name -> Just name
          Nothing   -> rtImportedPayee t

-- | Build the base argument list for all `actual` CLI invocations.
baseArgs :: AppSettings -> [String]
baseArgs settings =
  [ "--server-url", T.unpack (server_url (actual_settings settings))
  , "--password",   T.unpack (password   (actual_settings settings))
  , "--sync-id",    T.unpack (sync_id    (actual_settings settings))
  , "--data-dir",   data_dir settings
  , "--format",     "json"
  ] ++ encArg
  where
    encArg = maybe [] (\ep -> ["--encryption-password", T.unpack ep])
                      (encryption_password (actual_settings settings))

-- | Call `actual` and parse the JSON output as `[a]`.
callCli :: FromJSON a => AppSettings -> [String] -> IO [a]
callCli settings subArgs = do
  let args = baseArgs settings ++ subArgs
  currentEnv <- M.fromList <$> getEnvironment
  let nodeEnv = case ca_cert settings of
        Nothing   -> currentEnv
        Just cert -> M.insert "NODE_EXTRA_CA_CERTS" cert currentEnv
  let p = (proc "actual" args) { env = Just (M.toList nodeEnv) }
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode p ""
  case exitCode of
    ExitFailure n -> fail $
      "actual CLI failed (exit " ++ show n ++ ") for args " ++ show subArgs
      ++ "\nstderr: " ++ take 500 stderr
    ExitSuccess   ->
      case eitherDecode (BL.fromStrict (TE.encodeUtf8 (T.pack stdout))) of
        Left err -> fail $
          "Failed to parse actual CLI JSON for " ++ show subArgs
          ++ ": " ++ err
          ++ "\n(output preview): " ++ take 300 stdout
        Right xs  -> return xs

-- | Convert the year filter to a (start, end) date range as YYYYMMDD integers.
dateRangeInt :: Maybe String -> (Int, Int)
dateRangeInt Nothing        = (19000101, 20991231)
dateRangeInt (Just "all")   = (19000101, 20991231)
dateRangeInt (Just y)       = (read (y ++ "0101"), read (y ++ "1231"))
