{-# LANGUAGE RecordWildCards #-}

module Ynab.ReqApp (getAccountsApp, getPayeesApp) where

import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask))
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Ynab (YnabApp)
import Ynab.Req (getItemList)
import Ynab.Types

getItemsApp :: (FromJSON a, PayloadItems a) => Text -> Maybe Text -> YnabApp ([a], Text)
getItemsApp itemTypeName maybeSK = do
  AppEnv {..} <- ask
  let apiSettings = ynab_api_settings appSettings
  liftIO $ getItemList apiSettings baseURL budgetId itemTypeName maybeSK

getAccountsApp :: Maybe Text -> YnabApp ([Account], Text)
getAccountsApp = getItemsApp "accounts"

getPayeesApp :: Maybe Text -> YnabApp ([Payee], Text)
getPayeesApp = getItemsApp "payees"
