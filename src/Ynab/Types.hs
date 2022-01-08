module Ynab.Types where

import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import Network.HTTP.Req ( Scheme(Https), Url )

data CurrencyFormat = CurrencyFormat
    { iso_code :: Text
    , example_format :: Text
    ,  decimal_digits :: Int
    , decimal_separator :: Text
    , symbol_first :: Bool
    , group_separator :: Text
    , currency_symbol :: Text
    , display_symbol :: Bool }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Account = Account
    { account_id :: Text
    , account_deleted :: Bool
    , account_name :: Text
    , type_ :: Text
    , on_budget :: Bool
    , closed :: Bool
    , note :: Maybe Text
    , balance :: Int
    , cleared_balance :: Int
    , uncleared_balance :: Int
    , transfer_payee_id :: Text
    , direct_import_linked :: Bool
    , direct_import_in_error :: Bool }
    deriving (Eq, Show, Generic)

instance FromJSON Account where
    parseJSON = genericParseJSON
        defaultOptions { omitNothingFields = True
                       , fieldLabelModifier = accountMapFieldName }
        where
            accountMapFieldName :: String -> String
            accountMapFieldName "account_id" = "id"
            accountMapFieldName "account_name" = "name"
            accountMapFieldName "account_deleted" = "deleted"
            accountMapFieldName "type_" = "type"
            accountMapFieldName s = s

newtype DateFormat = DateFormat {format :: Text}
    deriving (Eq, Show, Generic, FromJSON)

data Budget = Budget
    { budget_id :: Text
    , budget_name_ :: Text
    , last_modified_on :: Text  -- TODO: should be date & time!!
    , first_month :: Text
    , last_month :: Text
    , date_format :: DateFormat
    , currency_format :: CurrencyFormat
    , accounts :: Maybe [Account] }
    deriving (Eq, Show, Generic)

instance FromJSON Budget where
    parseJSON = genericParseJSON
        defaultOptions { omitNothingFields = True
                       , fieldLabelModifier = budgetMapFieldName }
        where
            budgetMapFieldName :: String -> String
            budgetMapFieldName "budget_id" = "id"
            budgetMapFieldName "budget_name_" = "name"
            budgetMapFieldName s = s

data Payee = Payee
    { payee_id :: Text
    , payee_name :: Text
    , transfer_account_id :: Maybe Text
    , payee_deleted :: Bool }
    deriving (Eq, Show, Generic)

instance FromJSON Payee where
    parseJSON = genericParseJSON
        defaultOptions { omitNothingFields = True
                       , fieldLabelModifier = payeeMapFieldName }
        where
            payeeMapFieldName :: String -> String
            payeeMapFieldName "payee_id" = "id"
            payeeMapFieldName "payee_name" = "name"
            payeeMapFieldName "payee_deleted" = "deleted"
            payeeMapFieldName "type_" = "type"
            payeeMapFieldName s = s

type Address = Text
type Token = Text

data YnabApiSettings = YnabApiSettings
    { api_base_url :: Address, api_token :: Token }
    deriving (Eq, Show, Generic, FromJSON)

data AppSettings = AppSettings
    { ynab_api_settings :: YnabApiSettings
    , budget_name :: Text
    , starting_balance_account :: Text
    , transfer_account :: Text
    , account_map :: Map.Map Text Text }
    deriving (Eq, Show, Generic, FromJSON)

decodeFromJSON :: FromJSON a => Text -> Maybe a
decodeFromJSON content = decode $ fromStrict $ encodeUtf8 content

data AppEnv = AppEnv
    { appSettings :: AppSettings
    , baseURL :: Url 'Https
    , dbConn :: Connection }
