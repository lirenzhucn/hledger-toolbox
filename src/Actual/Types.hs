module Actual.Types where

import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

-- Settings

data ActualSettings = ActualSettings
  { server_url          :: Text
  , password            :: Text
  , encryption_password :: Maybe Text
  , sync_id             :: Text
  } deriving (Eq, Show, Generic, FromJSON)

data AppSettings = AppSettings
  { actual_settings          :: ActualSettings
  , data_dir                 :: FilePath
  , ca_cert                  :: Maybe FilePath
  , starting_balance_account :: Text
  , transfer_account         :: Text
  , account_map              :: Map.Map Text Text
  , payee_map                :: Map.Map Text Text
  } deriving (Eq, Show)

-- JSON-parsing types: all credential fields are optional so the config file
-- can be committed to git without secrets.

data PartialActualSettings = PartialActualSettings
  { pasServerUrl          :: Maybe Text
  , pasPassword           :: Maybe Text
  , pasEncryptionPassword :: Maybe Text
  , pasSyncId             :: Maybe Text
  } deriving (Eq, Show)

instance FromJSON PartialActualSettings where
  parseJSON = withObject "ActualSettings" $ \v -> PartialActualSettings
    <$> v .:? "server_url"
    <*> v .:? "password"
    <*> v .:? "encryption_password"
    <*> v .:? "sync_id"

data AppSettingsJSON = AppSettingsJSON
  { asjActualSettings         :: PartialActualSettings
  , asjDataDir                :: FilePath
  , asjCaCert                 :: Maybe FilePath
  , asjStartingBalanceAccount :: Text
  , asjTransferAccount        :: Text
  , asjAccountMap             :: Map.Map Text Text
  , asjPayeeMap               :: Map.Map Text Text
  } deriving (Eq, Show)

instance FromJSON AppSettingsJSON where
  parseJSON = withObject "AppSettings" $ \v -> AppSettingsJSON
    <$> (v .:? "actual_settings" .!= PartialActualSettings Nothing Nothing Nothing Nothing)
    <*> v .:  "data_dir"
    <*> v .:? "ca_cert"
    <*> v .:  "starting_balance_account"
    <*> v .:  "transfer_account"
    <*> v .:  "account_map"
    <*> v .:  "payee_map"

-- Raw API types: field names match the actual-cli JSON output exactly.

data RawAccount = RawAccount
  { raId     :: Text
  , raName   :: Text
  , raClosed :: Bool
  } deriving (Eq, Show)

instance FromJSON RawAccount where
  parseJSON = withObject "Account" $ \v -> RawAccount
    <$> v .:  "id"
    <*> v .:  "name"
    <*> v .:? "closed" .!= False

data RawPayee = RawPayee
  { rpId   :: Text
  , rpName :: Text
  } deriving (Eq, Show)

instance FromJSON RawPayee where
  parseJSON = withObject "Payee" $ \v -> RawPayee
    <$> v .: "id"
    <*> v .: "name"

data RawCategory = RawCategory
  { rcId       :: Text
  , rcName     :: Text
  , rcIsIncome :: Bool
  } deriving (Eq, Show)

instance FromJSON RawCategory where
  parseJSON = withObject "Category" $ \v -> RawCategory
    <$> v .:  "id"
    <*> v .:  "name"
    <*> v .:? "is_income" .!= False

data RawCategoryGroup = RawCategoryGroup
  { rcgId         :: Text
  , rcgName       :: Text
  , rcgIsIncome   :: Bool
  , rcgCategories :: [RawCategory]
  } deriving (Eq, Show)

instance FromJSON RawCategoryGroup where
  parseJSON = withObject "CategoryGroup" $ \v -> RawCategoryGroup
    <$> v .:  "id"
    <*> v .:  "name"
    <*> v .:? "is_income"  .!= False
    <*> v .:? "categories" .!= []

-- Raw transaction as returned by `actual transactions list --format json`.
-- The same type covers both top-level transactions (rtSubtransactions may be
-- non-empty for split parents) and the objects inside subtransactions arrays
-- (where rtSubtransactions will always be []).
data RawTx = RawTx
  { rtId                  :: Text
  , rtAccount             :: Text        -- account UUID
  , rtDate                :: Day
  , rtAmount              :: Int         -- cents; negative = outflow
  , rtPayee               :: Maybe Text  -- payee UUID
  , rtCategory            :: Maybe Text  -- category UUID
  , rtNotes               :: Maybe Text
  , rtImportedPayee       :: Maybe Text
  , rtCleared             :: Bool
  , rtTransferId          :: Maybe Text  -- non-null: one leg of a transfer
  , rtStartingBalanceFlag :: Bool
  , rtIsParent            :: Bool        -- split parent: skip, use subtransactions
  , rtIsChild             :: Bool
  , rtSubtransactions     :: [RawTx]
  } deriving (Eq, Show)

instance FromJSON RawTx where
  parseJSON = withObject "RawTx" $ \v -> RawTx
    <$> v .:  "id"
    <*> v .:  "account"
    <*> v .:  "date"
    <*> v .:  "amount"
    <*> v .:? "payee"
    <*> v .:? "category"
    <*> v .:? "notes"
    <*> v .:? "imported_payee"
    <*> v .:? "cleared"               .!= False
    <*> v .:? "transfer_id"
    <*> v .:? "starting_balance_flag" .!= False
    <*> v .:? "is_parent"             .!= False
    <*> v .:? "is_child"              .!= False
    <*> v .:? "subtransactions"       .!= []

-- Enriched transaction: UUIDs resolved to display names.
-- This is what Actual.Hledger converts to hledger format.
data Transaction = Transaction
  { txId              :: Text
  , txAccountName     :: Text
  , txDate            :: Day
  , txAmount          :: Int
  , txPayeeName       :: Maybe Text
  , txCategoryName    :: Maybe Text
  , txCategoryGroup   :: Maybe Text
  , txIsIncomeCat     :: Bool
  , txNotes           :: Maybe Text
  , txCleared         :: Bool
  , txTransferId      :: Maybe Text
  , txStartingBalance :: Bool
  } deriving (Eq, Show)
