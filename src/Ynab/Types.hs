{-# LANGUAGE FlexibleContexts #-}

module Ynab.Types where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Tx
import Data.Text.Encoding (encodeUtf8)
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
  deriving (Eq, Show, Generic, FromJSON)

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
    categoryCategoryGroupId :: Text,
    categoryName :: Text,
    categoryNote :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance PayloadItems Category where
  isKeyValid _ k = k == "categories"

instance FromJSON Category where
  parseJSON = parseJSONForPayloadTypes' "category"

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
