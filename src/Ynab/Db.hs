{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Ynab.Db where

import Control.Monad (unless)
import Control.Monad.Catch (Exception, MonadThrow (..), handle)
import Control.Monad.State (get, liftIO, modify)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Tx
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    Query (..),
    SQLData (..),
    SQLError,
    close,
    execute,
    executeMany,
    execute_,
    open,
    query_,
  )
import Database.SQLite.Simple.FromRow (FromRow (..), RowParser, field)
import Database.SQLite.Simple.ToRow (ToRow (..))
import System.Directory (doesFileExist)
import System.FilePath (FilePath, (<.>), (</>))
import Ynab.Types

dbFilePath :: FilePath -> Text -> FilePath
dbFilePath baseDir budgetId = baseDir </> Tx.unpack budgetId <.> "dat"

-- initialize database connection in app state
-- reseed the database if the file does not exist
initDbConn :: FilePath -> Text -> IO Connection
initDbConn baseDir budgetId = do
  let fpath = dbFilePath baseDir budgetId
  dbFileExists <- doesFileExist fpath
  conn <- open fpath
  unless dbFileExists $ reseedDb conn
  pure conn

-- rename the export in case we need to add more steps
closeDbConn = close

reseedDb :: Connection -> IO ()
reseedDb conn = do
  execute_
    conn
    "CREATE TABLE server_knowledge (\
    \    type_ TEXT PRIMARY KEY, value TEXT\
    \)"
  execute_
    conn
    "CREATE TABLE accounts (\
    \    id TEXT PRIMARY KEY,\
    \    deleted BOOL,\
    \    name TEXT,\
    \    type_ TEXT,\
    \    on_budget BOOL,\
    \    closed BOOL,\
    \    note TEXT,\
    \    balance INT,\
    \    transfer_payee_id TEXT\
    \)"
  execute_
    conn
    "CREATE TABLE payees (\
    \    id TEXT PRIMARY KEY,\
    \    name TEXT,\
    \    transfer_account_id TEXT,\
    \    deleted BOOL\
    \)"
  execute_
    conn
    "CREATE TABLE categories (\
    \    id TEXT PRIMARY KEY,\
    \    deleted BOOL,\
    \    name TEXT,\
    \    note TEXT,\
    \    group_id TEXT,\
    \    group_name TEXT\
    \)"
  execute_
    conn
    "CREATE TABLE transactions (\
    \    id TEXT PRIMARY KEY,\
    \    deleted BOOL,\
    \    amount INT,\
    \    date TEXT,\
    \    cleared TEXT,\
    \    approved BOOL,\
    \    account_id TEXT,\
    \    account_name TEXT,\
    \    payee_id TEXT,\
    \    payee_name TEXT,\
    \    category_id TEXT,\
    \    category_name TEXT,\
    \    transfer_account_id TEXT,\
    \    transfer_transaction_id TEXT,\
    \    memo TEXT,\
    \    children TEXT\
    \)"

getServerKnowledgeFromDb :: Connection -> IO ServerKnowledgeSet
getServerKnowledgeFromDb conn = do
  rowsAccounts <- runQuery "accounts"
  rowsPayees <- runQuery "payees"
  rowsCategoryGroups <- runQuery "category_groups"
  rowsTransactions <- runQuery "transactions"
  pure $
    ServerKnowledgeSet
      (rowsToSK rowsAccounts)
      (rowsToSK rowsPayees)
      (rowsToSK rowsCategoryGroups)
      (rowsToSK rowsTransactions)
  where
    runQuery :: Text -> IO [Only Text]
    runQuery type_ =
      query_
        conn
        $ Query ("SELECT value FROM server_knowledge WHERE type_='" <> type_ <> "'")
    rowsToSK :: [Only Text] -> Maybe Text
    rowsToSK [] = Nothing
    rowsToSK ((Only "") : _) = Nothing
    rowsToSK ((Only x) : _) = Just x

setServerKnowledgeToDb :: Connection -> ServerKnowledgeSet -> IO ()
setServerKnowledgeToDb conn sks = do
  let ServerKnowledgeSet {..} = sks
  executeMany
    conn
    "INSERT OR REPLACE INTO server_knowledge (type_, value) VALUES (?, ?)"
    [ ("accounts" :: Text, fromMaybe "" serverKnowledgeAccounts),
      ("payees" :: Text, fromMaybe "" serverKnowledgePayees),
      ("category_groups" :: Text, fromMaybe "" serverKnowledgeCategoryGroups),
      ("transactions" :: Text, fromMaybe "" serverKnowledgeTransactions)
    ]

boolToInt :: (Integral a) => Bool -> a
boolToInt b = if b then 1 else 0

boolToSQLData :: Bool -> SQLData
boolToSQLData = SQLInteger . boolToInt

instance ToRow Account where
  toRow :: Account -> [SQLData]
  toRow Account {..} =
    [ SQLText accountId,
      boolToSQLData accountDeleted,
      SQLText accountName,
      SQLText accountType,
      boolToSQLData accountOnBudget,
      boolToSQLData accountClosed,
      SQLText $ fromMaybe "" accountNote,
      SQLInteger $ fromIntegral accountBalance,
      SQLText accountTransferPayeeId
    ]

insertAccounts :: Connection -> [Account] -> IO ()
insertAccounts =
  flip
    executeMany
    "INSERT OR REPLACE INTO accounts (id, deleted, name, type_, on_budget, \
    \closed, note, balance, transfer_payee_id) VALUES (?,?,?,?,?,?,?,?,?)"

instance ToRow Payee where
  toRow :: Payee -> [SQLData]
  toRow Payee {..} =
    [ SQLText payeeId,
      SQLText payeeName,
      SQLText $ fromMaybe "" payeeTransferAccountId,
      boolToSQLData payeeDeleted
    ]

insertPayees :: Connection -> [Payee] -> IO ()
insertPayees =
  flip
    executeMany
    "INSERT OR REPLACE INTO payees (id, name, transfer_account_id, deleted) \
    \VALUES (?,?,?,?)"

instance ToRow Category where
  toRow :: Category -> [SQLData]
  toRow Category {..} =
    [ SQLText categoryId,
      boolToSQLData categoryDeleted,
      SQLText categoryName,
      SQLText $ fromMaybe "" categoryNote,
      SQLText categoryCategoryGroupId,
      SQLText $ fromMaybe "" categoryCategoryGroupName
    ]

instance FromRow Category where
  fromRow :: RowParser Category
  fromRow =
    Category
      <$> field
      <*> field
      <*> field
      <*> (toMaybeText <$> field)
      <*> field
      <*> (toMaybeText <$> field)

insertCategories :: Connection -> [Category] -> IO ()
insertCategories =
  flip
    executeMany
    "INSERT OR REPLACE INTO categories (id, deleted, name, note, group_id, \
    \group_name) VALUES (?,?,?,?,?,?)"

getAllCategories :: Connection -> IO [Category]
getAllCategories =
  flip
    query_
    "SELECT id, deleted, name, note, group_id, group_name from categories"

instance ToRow TransactionDb where
  toRow :: TransactionDb -> [SQLData]
  toRow TransactionDb {..} =
    [ SQLText trdId,
      boolToSQLData trdDeleted,
      SQLInteger $ fromIntegral trdAmount,
      SQLText trdDate,
      SQLText trdCleared,
      boolToSQLData trdApproved,
      SQLText trdAccountId,
      SQLText trdAccountName,
      SQLText $ fromMaybe "" trdPayeeId,
      SQLText $ fromMaybe "" trdPayeeName,
      SQLText $ fromMaybe "" trdCategoryId,
      SQLText $ fromMaybe "" trdCategoryName,
      SQLText $ fromMaybe "" trdTransferAccountId,
      SQLText $ fromMaybe "" trdTransferTransactionId,
      SQLText $ fromMaybe "" trdMemo,
      SQLText $ Tx.unwords trdChildrenIds
    ]

instance FromRow TransactionDb where
  fromRow :: RowParser TransactionDb
  fromRow =
    TransactionDb
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> (toMaybeText <$> field)
      <*> (toMaybeText <$> field)
      <*> (toMaybeText <$> field)
      <*> (toMaybeText <$> field)
      <*> (toMaybeText <$> field)
      <*> (toMaybeText <$> field)
      <*> (toMaybeText <$> field)
      <*> (Tx.words <$> field)

insertTransactions :: Connection -> [TransactionDb] -> IO ()
insertTransactions =
  flip
    executeMany
    "INSERT OR REPLACE INTO transactions (id, deleted, amount, date, cleared, \
    \approved, account_id, account_name, payee_id, payee_name, category_id, \
    \category_name, transfer_account_id, transfer_transaction_id, memo, \
    \children) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"

getAllTransactions :: Connection -> IO [TransactionDb]
getAllTransactions =
  flip
    query_
    "SELECT id, deleted, amount, date, cleared, approved, account_id, \
    \account_name, payee_id, payee_name, category_id, category_name, \
    \transfer_account_id, transfer_transaction_id, memo, children FROM \
    \transactions ORDER BY date"

toMaybeText :: Text -> Maybe Text
toMaybeText "" = Nothing
toMaybeText s = Just s
