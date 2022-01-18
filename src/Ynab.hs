{-# LANGUAGE RecordWildCards #-}

module Ynab where

import Control.Monad.Logger (runStderrLoggingT, logInfoN)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Time (showGregorian)
import Network.HTTP.Req (Scheme (Https), Url, https, useHttpsURI, (/:))
import Text.URI (URI, mkURI)
import TextShow (showt)
import Ynab.Db
import Ynab.Req (getBudget)
import Ynab.ReqApp
  ( getAccountsApp,
    getCategoryGroupsApp,
    getPayeesApp,
    getTransactionsApp,
  )
import Ynab.Types

runYnabApp :: YnabApp a -> AppEnv -> IO a
runYnabApp app env = runStderrLoggingT (runReaderT (runApp app) env)

initEnv :: AppSettings -> IO AppEnv
initEnv settings = do
  let url = fromMaybe defaultURL maybeURL
  budget <- getBudget settings url
  let _budgetId = budgetId_ budget
  conn <- initDbConn ".secrets/dbs" (budgetId_ budget)
  pure
    AppEnv
      { appSettings = settings,
        baseURL = url,
        budgetId = _budgetId,
        dbConn = conn
      }
  where
    maybeURL = makeURL $ api_base_url $ ynab_api_settings settings
    defaultURL = https "api.youneedabudget.com" /: "v1"

makeURL :: Text -> Maybe (Url 'Https)
makeURL urlText =
  pure urlText >>= mkURI >>= useHttpsURI >>= (\(ep, _) -> Just ep)

fetchData :: YnabApp ()
fetchData = do
  AppEnv {..} <- ask
  -- get server knowledge
  ServerKnowledgeSet {..} <- liftIO $ getServerKnowledgeFromDb dbConn
  -- get info from endpoints
  (accounts, skAccounts) <- getAccountsApp serverKnowledgeAccounts
  (payees, skPayees) <- getPayeesApp serverKnowledgePayees
  (categoryGroups, skCategoryGroups) <- getCategoryGroupsApp serverKnowledgeCategoryGroups
  let categories = makeCategories categoryGroups
  (transactions, skTransactions) <- getTransactionsApp serverKnowledgeTransactions
  dbTrans <- makeDbTrans transactions
  -- log number of entities pulled from API
  logInfoN $ showt (length accounts) <> " account(s) pulled"
  logInfoN $ showt (length payees) <> " payee(s) pulled"
  logInfoN $ showt (length categoryGroups) <> " category group(s) pulled"
  logInfoN $ showt (length dbTrans) <> " transaction(s) pulled"
  -- write everything to db
  liftIO $ insertAccounts dbConn accounts
  liftIO $ insertPayees dbConn payees
  liftIO $ insertCategories dbConn categories
  liftIO $ insertTransactions dbConn dbTrans
  liftIO $
    setServerKnowledgeToDb
      dbConn
      ServerKnowledgeSet
        { serverKnowledgeAccounts = Just skAccounts,
          serverKnowledgePayees = Just skPayees,
          serverKnowledgeCategoryGroups = Just skCategoryGroups,
          serverKnowledgeTransactions = Just skTransactions
        }
  --
  pure ()
  where
    makeCategories :: [CategoryGroup] -> [Category]
    makeCategories cgs =
      foldl
        ( \c0 cg ->
            c0
              ++ fmap
                (fillCGName $ categoryGroupName cg)
                (categoryGroupCategories cg)
        )
        []
        cgs
    fillCGName :: Text -> Category -> Category
    fillCGName cgName c = c {categoryCategoryGroupName = Just cgName}
    makeDbTrans :: [Transaction] -> YnabApp [TransactionDb]
    makeDbTrans ts = do
      pure $
        foldl
          ( \t0 t ->
              t0 ++ [trToTrd (getChildrenIds t) (trDetails t)]
                ++ fmap
                  (trToTrd [] . replaceFields (trDetails t) . stTrDetails)
                  (trSubTrans t)
          )
          []
          ts
    getChildrenIds :: Transaction -> [Text]
    getChildrenIds trans = map (tdId . stTrDetails) (trSubTrans trans)
    replaceFields :: TransactionDetails -> TransactionDetails -> TransactionDetails
    replaceFields template target =
      target
        { tdDate = tdDate template,
          tdCleared = tdCleared template,
          tdApproved = tdApproved template,
          tdAccountId = tdAccountId template,
          tdAccountName = tdAccountName template
        }
    trToTrd :: [Text] -> TransactionDetails -> TransactionDb
    trToTrd childrenIds TransactionDetails {..} =
      TransactionDb
        { trdId = tdId,
          trdDeleted = tdDeleted,
          trdAmount = tdAmount,
          trdDate = pack $ maybe "" showGregorian tdDate,
          trdCleared = fromMaybe "" tdCleared,
          trdApproved = fromMaybe True tdApproved,
          trdAccountId = fromMaybe "" tdAccountId,
          trdAccountName = fromMaybe "" tdAccountName,
          trdPayeeId = tdPayeeId,
          trdPayeeName = tdPayeeName,
          trdCategoryId = tdCategoryId,
          trdCategoryName = tdCategoryName,
          trdTransferAccountId = tdTransferAccountId,
          trdTransferTransactionId = tdTransferTransactionId,
          trdMemo = tdMemo,
          trdChildrenIds = childrenIds
        }

writeJournal :: YnabApp ()
writeJournal = do
  AppEnv {..} <- ask
  pure ()
