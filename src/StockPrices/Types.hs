{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module StockPrices.Types where

import Data.Aeson ( (.:)
                  , (.:?)
                  , (<?>)
                  , FromJSON (..)
                  , Object
                  , Value (..)
                  , withObject
                  )
import Data.Aeson.Key ( Key(..), toText )
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types ( JSONPathElement (..), Parser )
import Data.Char ( toLower )
import Data.Decimal ( Decimal )
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Time ( Day, defaultTimeLocale, parseTimeM )
import GHC.Generics ( Generic )
import Hledger ( CommoditySymbol )
import Text.Read ( readMaybe )

data PriceDataFrequency = Daily | Weekly | Monthly
  deriving (Eq, Show)

instance Read PriceDataFrequency where
  readsPrec _ input = case (map toLower input) of
    "daily" -> [(Daily, "")]
    "weekly" -> [(Weekly, "")]
    "monthly" -> [(Monthly, "")]
    _ -> []

newtype AlphaVantageConfig = AlphaVantageConfig { cApiKey :: T.Text }
  deriving (Show, Read, Eq, Generic)

data AlphaVantageResponse a = ApiResponse a | ApiError T.Text
  deriving (Show, Read, Eq, Generic, Functor)

instance FromJSON a => FromJSON (AlphaVantageResponse a) where
  parseJSON = withObject "AlphaVantageResponse" $ \v -> do
    mbErrorNote <- v .:? "Note"
    case mbErrorNote of
      Nothing -> ApiResponse <$> parseJSON (Object v)
      Just note -> return $ ApiError note

data Prices = Prices
  { pOpen :: Decimal
  , pHigh :: Decimal
  , pLow :: Decimal
  , pClose :: Decimal
  , pVolume :: Integer
  }
  deriving (Show, Read, Eq, Generic)

parseAlphaVantageDay :: String -> Parser Day
parseAlphaVantageDay = parseTimeM True defaultTimeLocale "%F"

parseDecimal :: (MonadFail m, Read a) => m String -> m a
parseDecimal parser = do
  val <- parser
  case readMaybe val of
    Just x -> return x
    Nothing -> fail $ "could not parse number: " ++ val

instance FromJSON Prices where
  parseJSON = withObject "Prices" $ \v -> do
    pOpen <- parseDecimal $ v .: "1. open"
    pHigh <- parseDecimal $ v .: "2. high"
    pLow <- parseDecimal $ v .: "3. low"
    pClose <- parseDecimal $ v .: "4. close"
    pVolume <- parseDecimal $ v .: "5. volume"
    return Prices { .. }

data CryptoPrices = CryptoPrices
  { cpOpen :: Decimal
  , cpHigh :: Decimal
  , cpLow :: Decimal
  , cpClose :: Decimal
  , cpMarketCap :: Decimal
  }
  deriving (Show, Read, Eq, Generic)

instance FromJSON CryptoPrices where
  parseJSON = withObject "CryptoPrices" $ \v -> do
    cpOpen <- parseDecimal $ v .: "1b. open (USD)"
    cpHigh <- parseDecimal $ v .: "2b. high (USD)"
    cpLow <- parseDecimal $ v .: "3b. low (USD)"
    cpClose <- parseDecimal $ v .: "4b. close (USD)"
    cpMarketCap <- parseDecimal $ v .: "6. market cap (USD)"
    return CryptoPrices { .. }

newtype PriceList = PriceList { fromPriceList :: [(Day, Prices)] }
  deriving (Show, Read, Eq, Generic)

explicitParseFieldPrefix :: (Value -> Parser a) -> Object -> T.Text -> Parser a
explicitParseFieldPrefix p obj prefix =
  case  ( KM.toList $
          KM.filterWithKey
            (\k _ -> T.isInfixOf prefix $ toText k)
            obj
        ) of
    [(k,v)] -> p v <?> Key k
    (k,v):_ -> p v <?> Key k
    _ -> fail $ "key with prefix `" ++ (T.unpack prefix) ++ "` not found"


instance FromJSON PriceList where
  parseJSON = withObject "PriceList" $ \v -> do
    inner <- explicitParseFieldPrefix parseJSON v "Time Series"
    let daysAndPrices = HM.toList inner
    PriceList <$> mapM
      (\(d, ps) -> (,) <$> parseAlphaVantageDay d <*> parseJSON ps)
      daysAndPrices

newtype CryptoPriceList = CryptoPriceList
  { fromCryptoPriceList :: [(Day, CryptoPrices)] }
  deriving (Show, Read, Eq, Generic)

instance FromJSON CryptoPriceList where
  parseJSON = withObject "CryptoPriceList" $ \v -> do
    inner <- explicitParseFieldPrefix parseJSON v "Time Series"
    let daysAndPrices = HM.toList inner
    CryptoPriceList <$> mapM
      (\(d, ps) -> (,) <$> parseAlphaVantageDay d <*> parseJSON ps)
      daysAndPrices

data AlphaVantageRequest
  = FetchStock (CommoditySymbol, Day, Day)
  | FetchCrypto (CommoditySymbol, Day, Day)

data GenericPrice = Stock Prices | Crypto CryptoPrices
