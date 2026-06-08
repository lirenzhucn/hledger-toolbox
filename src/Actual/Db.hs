{-# OPTIONS_GHC -Wno-orphans #-}
module Actual.Db (fetchAllFromDb) where

import Actual.Types
import Control.Exception (bracket)
import Data.Aeson (FromJSON (..), decodeStrict, withObject, (.:))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (RowParser)
import System.FilePath ((</>))

-- | actual-cli writes <dataDir>/.actual-cli/<syncId>/state.json after any sync.
-- That file contains {"budgetId": "<name-hash>"} which is the subdirectory
-- under dataDir where the budget's db.sqlite lives.
newtype CliState = CliState { cliStateBudgetId :: String }

instance FromJSON CliState where
  parseJSON = withObject "CliState" $ \v -> CliState <$> v .: "budgetId"

findDbPath :: FilePath -> Text -> IO FilePath
findDbPath dataDir syncId = do
  let stateFile = dataDir </> ".actual-cli" </> T.unpack syncId </> "state.json"
  bs <- BS.readFile stateFile
  case decodeStrict bs of
    Just s  -> return (dataDir </> cliStateBudgetId s </> "db.sqlite")
    Nothing -> fail $ "Failed to parse CLI state file: " ++ stateFile

-- | Open the budget db.sqlite, run all queries, close. Caller supplies the
-- date range as YYYYMMDD integers.
fetchAllFromDb
  :: FilePath  -- data_dir
  -> Text      -- sync_id (used to locate the right budget subdir via state.json)
  -> (Int, Int)
  -> IO ([RawAccount], [RawPayee], [RawCategoryGroup], [RawTx])
fetchAllFromDb dir syncId (startDate, endDate) = do
  dbPath <- findDbPath dir syncId
  bracket (open dbPath) close $ \conn -> do
    accounts <- query_ conn accountsSql
    payees   <- query_ conn payeesSql
    catRows  <- query_ conn categoriesSql
    txs      <- query  conn txSql (startDate, endDate)
    return (accounts, payees, assembleCategoryGroups catRows, txs)

-- Orphan FromRow instances — only used by the queries below.

instance FromRow RawAccount where
  fromRow = do
    aid    <- field
    aname  <- field
    closed <- field :: RowParser Int
    return $ RawAccount aid aname (closed /= 0)

instance FromRow RawPayee where
  fromRow = RawPayee <$> field <*> field

data CatRow = CatRow
  { crCatId     :: Text
  , crCatName   :: Text
  , crCatIncome :: Bool
  , crGrpId     :: Text
  , crGrpName   :: Text
  , crGrpIncome :: Bool
  }

instance FromRow CatRow where
  fromRow = do
    catId  <- field
    catNm  <- field
    catInc <- field :: RowParser Int
    grpId  <- field
    grpNm  <- field
    grpInc <- field :: RowParser Int
    return $ CatRow catId catNm (catInc /= 0) grpId grpNm (grpInc /= 0)

assembleCategoryGroups :: [CatRow] -> [RawCategoryGroup]
assembleCategoryGroups rows =
  M.elems $ M.fromListWith merge
    [ ( crGrpId r
      , RawCategoryGroup (crGrpId r) (crGrpName r) (crGrpIncome r)
                         [RawCategory (crCatId r) (crCatName r) (crCatIncome r)]
      )
    | r <- rows
    ]
  where
    merge (RawCategoryGroup gid gn gi c1) (RawCategoryGroup _ _ _ c2) =
      RawCategoryGroup gid gn gi (c1 ++ c2)

instance FromRow RawTx where
  fromRow = do
    tid     <- field
    isP     <- field :: RowParser Int
    isC     <- field :: RowParser Int
    acct    <- field
    cat     <- field
    amt     <- field
    payee   <- field
    notes   <- field
    dateInt <- field :: RowParser Int
    impPay  <- field
    sbf     <- field :: RowParser Int
    trid    <- field
    clr     <- field :: RowParser Int
    return RawTx
      { rtId                  = tid
      , rtAccount             = acct
      , rtDate                = intToDay dateInt
      , rtAmount              = amt
      , rtPayee               = payee
      , rtCategory            = cat
      , rtNotes               = notes
      , rtImportedPayee       = impPay
      , rtCleared             = clr /= 0
      , rtTransferId          = trid
      , rtStartingBalanceFlag = sbf /= 0
      , rtIsParent            = isP /= 0
      , rtIsChild             = isC /= 0
      , rtSubtransactions     = []
      }

intToDay :: Int -> Day
intToDay n = fromGregorian (fromIntegral $ n `div` 10000)
                            ((n `mod` 10000) `div` 100)
                            (n `mod` 100)

accountsSql :: Query
accountsSql =
  "SELECT id, name, closed FROM accounts WHERE tombstone = 0"

payeesSql :: Query
payeesSql =
  "SELECT id, name FROM payees WHERE tombstone = 0"

categoriesSql :: Query
categoriesSql =
  "SELECT c.id, c.name, c.is_income, cg.id, cg.name, cg.is_income \
  \FROM categories c \
  \JOIN category_groups cg ON c.cat_group = cg.id \
  \WHERE c.tombstone = 0 AND cg.tombstone = 0"

-- Children inherit the parent's payee UUID via LEFT JOIN when their own
-- description is NULL (common for split transaction children).
txSql :: Query
txSql =
  "SELECT t.id, t.isParent, t.isChild, t.acct, t.category, t.amount, \
  \       COALESCE(t.description, p.description), t.notes, t.date, \
  \       t.imported_description, t.starting_balance_flag, \
  \       t.transferred_id, t.cleared \
  \FROM transactions t \
  \LEFT JOIN transactions p ON t.parent_id = p.id \
  \WHERE t.tombstone = 0 AND t.isParent = 0 \
  \  AND t.date >= ? AND t.date <= ? \
  \ORDER BY t.date, t.sort_order"
