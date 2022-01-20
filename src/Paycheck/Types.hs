module Paycheck.Types where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Regex.Base
import Text.Regex.PCRE ((=~))

data PostingConfig = PostingConfig
  { pcAccount2 :: Text,
    pcPattern :: Text,
    pcAmount :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON PostingConfig where
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = (camelTo2 '_') . (drop 2)
        }

newtype GlobalConfig = GlobalConfig { gcAccount1 :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON GlobalConfig where
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = (camelTo2 '_') . (drop 2)
        }

data AppSettings = AppSettings
  { asGlobal :: GlobalConfig,
    asPostings :: [PostingConfig]
  }
  deriving (Eq, Show, Generic)

instance FromJSON AppSettings where
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = (camelTo2 '_') . (drop 2)
        }