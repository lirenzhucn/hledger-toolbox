module Utils where

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

decodeFromJSON :: FromJSON a => Text -> Maybe a
decodeFromJSON = decode . fromStrict . encodeUtf8
