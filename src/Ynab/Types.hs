{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ynab.Types where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as AKM
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Tx
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (Day)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic, Rep)
import Network.HTTP.Req (Scheme (Https), Url)

-- type class that can be used as a payload item in the following json structure
-- {"data": {"<key>": [...]}}
-- instances provide a function isKeyValid to check <key> against instance type
-- e.g. list of Budgets should have a "budgets" key
class PayloadItems a where
  isKeyValid :: a -> String -> Bool

data CurrencyFormat = CurrencyFormat
  { iso_code :: Text,
    example_format :: Text,
    decimal_digits :: Int,
    decimal_separator :: Text,
    symbol_first :: Bool,
    group_separator :: Text,
    currency_symbol :: Text,
    display_symbol :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- a helper function used by the FromJSON instances of various payload types
parseJSONForPayloadTypes ::
  (Generic a, GFromJSON Zero (Rep a)) =>
  (String -> String) ->
  Text ->
  Value ->
  Parser a
parseJSONForPayloadTypes preMapper prefix =
  genericParseJSON
    defaultOptions
      { omitNothingFields = True,
        fieldLabelModifier = mapFieldName . preMapper
      }
  where
    mapFieldName :: String -> String
    mapFieldName s = camelTo2 '_' $ drop (Tx.length prefix) s

parseJSONForPayloadTypes' ::
  (Generic a, GFromJSON Zero (Rep a)) =>
  Text ->
  Value ->
  Parser a
parseJSONForPayloadTypes' = parseJSONForPayloadTypes id

data Account = Account
  { accountId :: Text,
    accountDeleted :: Bool,
    accountName :: Text,
    accountType :: Text,
    accountOnBudget :: Bool,
    accountClosed :: Bool,
    accountNote :: Maybe Text,
    accountBalance :: Int,
    accountTransferPayeeId :: Text
  }
  deriving (Eq, Show, Generic)

instance PayloadItems Account where
  isKeyValid _ k = k == "accounts"

instance FromJSON Account where
  parseJSON = parseJSONForPayloadTypes' "account"

newtype DateFormat = DateFormat {format :: Text}
  deriving (Eq, Show, Generic)
  deriving anyclass FromJSON

data Budget = Budget {budgetId_ :: Text, budgetName :: Text}
  deriving (Eq, Show, Generic)

instance PayloadItems Budget where
  isKeyValid _ k = k == "budgets"

instance FromJSON Budget where
  parseJSON = parseJSONForPayloadTypes preMapper "budget"
    where
      preMapper :: String -> String
      preMapper "budgetId_" = "budgetId"
      preMapper s = s

data Payee = Payee
  { payeeId :: Text,
    payeeName :: Text,
    payeeTransferAccountId :: Maybe Text,
    payeeDeleted :: Bool
  }
  deriving (Eq, Show, Generic)

instance PayloadItems Payee where
  isKeyValid _ k = k == "payees"

instance FromJSON Payee where
  parseJSON = parseJSONForPayloadTypes' "payee"

data Category = Category
  { categoryId :: Text,
    categoryDeleted :: Bool,
    categoryName :: Text,
    categoryNote :: Maybe Text,
    categoryCategoryGroupId :: Text,
    categoryCategoryGroupName :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Category where
  parseJSON = parseJSONForPayloadTypes' "category"

data CategoryGroup = CategoryGroup
  { categoryGroupId :: Text,
    categoryGroupDeleted :: Bool,
    categoryGroupName :: Text,
    categoryGroupCategories :: [Category]
  }
  deriving (Eq, Show, Generic)

instance PayloadItems CategoryGroup where
  isKeyValid _ k = k == "category_groups"

instance FromJSON CategoryGroup where
  parseJSON = parseJSONForPayloadTypes' "categoryGroup"

data TransactionDb = TransactionDb
  { trdId :: Text,
    trdDeleted :: Bool,
    trdAmount :: Int,
    trdDate :: Text,
    trdCleared :: Text,
    trdApproved :: Bool,
    trdAccountId :: Text,
    trdAccountName :: Text,
    trdPayeeId :: Maybe Text,
    trdPayeeName :: Maybe Text,
    trdCategoryId :: Maybe Text,
    trdCategoryName :: Maybe Text,
    trdTransferAccountId :: Maybe Text,
    trdTransferTransactionId :: Maybe Text,
    trdMemo :: Maybe Text,
    trdChildrenIds :: [Text]
  }
  deriving (Eq, Show)

data TransactionDetails = TransactionDetails
  { tdId :: Text,
    tdDeleted :: Bool,
    tdAmount :: Int,
    tdDate :: Maybe Day,
    tdCleared :: Maybe Text,
    tdApproved :: Maybe Bool,
    tdAccountId :: Maybe Text,
    tdAccountName :: Maybe Text,
    tdPayeeId :: Maybe Text,
    tdPayeeName :: Maybe Text,
    tdCategoryId :: Maybe Text,
    tdCategoryName :: Maybe Text,
    tdTransferAccountId :: Maybe Text,
    tdTransferTransactionId :: Maybe Text,
    tdMemo :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON TransactionDetails where
  parseJSON = parseJSONForPayloadTypes' "td"

data SubTransaction = SubTransaction
  { stTrDetails :: TransactionDetails,
    stTransactionId :: Text
  }
  deriving (Eq, Show)

instance FromJSON SubTransaction where
  parseJSON val = do
    td <- parseJSON val :: Parser TransactionDetails
    trId <- withObject "SubTransaction" extractTrId val
    pure SubTransaction {stTrDetails = td, stTransactionId = trId}
    where
      extractTrId :: Object -> Parser Text
      extractTrId v = do
        let maybeId = fmap parseJSON (AKM.lookup "transaction_id" v)
        case maybeId of
          Nothing  -> fail "A sub-transaction must have a transaction id"
          Just id_ -> id_

data Transaction = Transaction
  { trDetails :: TransactionDetails, trSubTrans :: [SubTransaction] }
  deriving (Eq, Show)

instance PayloadItems Transaction where
  isKeyValid _ k = k == "transactions"

instance FromJSON Transaction where
  parseJSON val = do
    td <- parseJSON val :: Parser TransactionDetails
    subTrans <- withObject "Transaction" extractSubTrans val
    pure Transaction {trDetails = td, trSubTrans = subTrans}
    where
      extractSubTrans :: Object -> Parser [SubTransaction]
      extractSubTrans v = do
        let maybeSubTrans = fmap parseJSON (AKM.lookup "subtransactions" v)
        case maybeSubTrans of
          Nothing       -> pure []
          Just subTrans -> subTrans

data ServerKnowledgeSet = ServerKnowledgeSet
  { serverKnowledgeAccounts :: Maybe Text,
    serverKnowledgePayees :: Maybe Text,
    serverKnowledgeCategoryGroups :: Maybe Text,
    serverKnowledgeTransactions :: Maybe Text
  }
  deriving (Eq, Show)

type Address = Text

type Token = Text

data YnabApiSettings = YnabApiSettings
  {api_base_url :: Address, api_token :: Token}
  deriving (Eq, Show, Generic, FromJSON)

data AppSettings = AppSettings
  { ynab_api_settings :: YnabApiSettings,
    budget_name :: Text,
    starting_balance_account :: Text,
    transfer_account :: Text,
    account_map :: Map.Map Text Text
  }
  deriving (Eq, Show, Generic, FromJSON)

decodeFromJSON :: FromJSON a => Text -> Maybe a
decodeFromJSON content = decode $ fromStrict $ encodeUtf8 content

data AppEnv = AppEnv
  { appSettings :: AppSettings,
    baseURL :: Url 'Https,
    budgetId :: Text,
    dbConn :: Connection
  }

newtype YnabApp a = YnabApp
  {runApp :: ReaderT AppEnv (LoggingT IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadLogger,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader AppEnv
    )
