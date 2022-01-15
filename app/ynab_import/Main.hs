module Main where

import Control.Monad.Catch (finally)
import Control.Monad.Reader (MonadIO (liftIO))
import qualified Data.Text.IO as TIO
import Ynab
import Ynab.Db
import Ynab.ReqApp (getAccountsApp, getPayeesApp)
import Ynab.Types

work :: AppSettings -> IO ()
work settings = do
  env <- initEnv settings
  finally (runYnabApp doWork env) (closeDbConn $ dbConn env)
  where
    doWork = do
      -- (accounts, sk) <- getAccountsApp Nothing
      (payees, sk) <- getPayeesApp Nothing
      liftIO $ putStrLn $ show payees

main :: IO ()
main = do
  content <- TIO.readFile "./.secrets/ynab_api_import_settings.json"
  let maybeSettings = (decodeFromJSON content :: Maybe AppSettings)
  case maybeSettings of
    Nothing -> putStrLn "Failed to parse settings"
    Just settings -> work settings
