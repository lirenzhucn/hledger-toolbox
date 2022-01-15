module Ynab.Types where

import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
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

data Account = Account
  { accountId :: Text,
    accountDeleted :: Bool,
    accountName :: Text,
    accountType :: Text,
    accountOnBudget :: Bool,
    accountClosed :: Bool,
    accountNote :: Maybe Text,
    accountBalance :: Int,
    accountClearedBalance :: Int,
    accountUnclearedBalance :: Int,
    accountTransferPayeeId :: Text,
    accountDirectImportLinked :: Bool,
    accountDirectImportInError :: Bool
  }
  deriving (Eq, Show, Generic)

instance PayloadItems Account where
  isKeyValid _ k = k == "accounts"

instance FromJSON Account where
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = accountMapFieldName
        }
    where
      accountMapFieldName :: String -> String
      accountMapFieldName s = camelTo2 '_' $ drop (length ("account" :: String)) s

newtype DateFormat = DateFormat {format :: Text}
  deriving (Eq, Show, Generic, FromJSON)

data Budget = Budget
  { budgetId_ :: Text,
    budgetName :: Text,
    budgetLastModifiedOn :: Text, -- TODO: should be date & time!!
    budgetFirstMonth :: Text,
    budgetLastMonth :: Text,
    budgetDateFormat :: DateFormat,
    budgetCurrencyFormat :: CurrencyFormat,
    budgetAccounts :: Maybe [Account]
  }
  deriving (Eq, Show, Generic)

instance PayloadItems Budget where
  isKeyValid _ k = k == "budgets"

instance FromJSON Budget where
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = budgetMapFieldName
        }
    where
      budgetMapFieldName :: String -> String
      budgetMapFieldName "budgetId_" = "id"
      budgetMapFieldName s = camelTo2 '_' $ drop (length ("budget" :: String)) s

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
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = payeeMapFieldName
        }
    where
      payeeMapFieldName :: String -> String
      payeeMapFieldName s = camelTo2 '_' $ drop (length ("payee" :: String)) s

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
