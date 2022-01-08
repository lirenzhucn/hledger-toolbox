module Ynab.Db (closeDbConn, initDbConn) where

import Control.Monad.Catch (Exception, MonadThrow(..), handle)
import Control.Monad (when)
import Control.Monad.State (get, modify, liftIO)
import Data.Text (Text, unpack)
import Database.SQLite.Simple (close, open, execute_, Connection, SQLError)
import System.Directory (doesFileExist)
import System.FilePath (FilePath, (</>), (<.>))
import Text.RawString.QQ
import Ynab.Types (AppEnv(..))

dbFilePath :: FilePath -> Text -> FilePath
dbFilePath baseDir budgetId = baseDir </> (unpack budgetId) <.> "dat"

-- initialize database connection in app state
-- reseed the database if the file does not exist
initDbConn :: FilePath -> Text -> IO Connection
initDbConn baseDir budgetId = do
    let fpath = dbFilePath baseDir budgetId
    dbFileExists <- doesFileExist fpath
    conn <- open fpath
    when (not dbFileExists) $ reseedDb conn
    pure conn

-- rename the export in case we need to add more steps
closeDbConn = close

reseedDb :: Connection -> IO ()
reseedDb conn = do
    execute_ conn [r|
    CREATE TABLE server_knowledge (
        type_ TEXT PRIMARY KEY, value TEXT
    )
    |]
    execute_ conn [r|
    CREATE TABLE payees (
        id TEXT PRIMARY KEY,
        name TEXT,
        transfer_account_id TEXT,
        deleted BOOL
    )
    |]
