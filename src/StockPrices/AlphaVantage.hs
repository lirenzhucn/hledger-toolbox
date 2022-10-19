{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module StockPrices.AlphaVantage where

import Data.Aeson ( (.:)
                  , (.:?)
                  , (<?>)
                  , FromJSON (..)
                  , Object
                  , Value (..)
                  , withObject
                  )
import Data.Aeson.Key ( Key(..), toText )
import Data.Aeson.Types ( JSONPathElement (..) )
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types ( Parser )
import Data.Char ( toLower )
import Data.Decimal ( Decimal )
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time ( Day, defaultTimeLocale, parseTimeM )
import GHC.Generics ( Generic )
import Network.HTTP.Req ( (/~)
                        , (=:)
                        , GET(..)
                        , NoReqBody(..)
                        , defaultHttpConfig
                        , https
                        , jsonResponse
                        , req
                        , responseBody
                        , runReq
                        )
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
  , cpVolume :: Decimal
  , cpMarketCap :: Decimal
  }
  deriving (Show, Read, Eq, Generic)

instance FromJSON CryptoPrices where
  parseJSON = withObject "CryptoPrices" $ \v -> do
    cpOpen <- parseDecimal $ v .: "1b. open (USD)"
    cpHigh <- parseDecimal $ v .: "2b. high (USD)"
    cpLow <- parseDecimal $ v .: "3b. low (USD)"
    cpClose <- parseDecimal $ v .: "4b. close (USD)"
    cpVolume <- parseDecimal $ v .: "5. volume (USD)"
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
    inner <- v .: "Time Series (Digital Currency Daily)"
    let daysAndPrices = HM.toList inner
    CryptoPriceList <$> mapM
      (\(d, ps) -> (,) <$> parseAlphaVantageDay d <*> parseJSON ps)
      daysAndPrices

filterByDate :: Day -> Day -> [(Day, a)] -> [(Day, a)]
filterByDate startDay endDay =
  takeWhile ((<= endDay) . fst)
    . dropWhile ((< startDay) . fst)
    . L.sortOn fst

data AlphaVantageGetType
  = StockGet { sgFunc :: T.Text }
  | CryptoGet { cgFunc :: T.Text, cgMarket :: T.Text }

getByFunctionAndFilter
  :: (FromJSON a, FromJSON b)
  => AlphaVantageGetType
  -> (a -> [(Day, b)])
  -> AlphaVantageConfig
  -> T.Text
  -> Day
  -> Day
  -> IO (AlphaVantageResponse [(Day, b)])
getByFunctionAndFilter getType f cfg symbol startDay endDay = do
  resp <- runReq defaultHttpConfig $ req
    GET
    (https "www.alphavantage.co" /~ ("query" :: T.Text))
    NoReqBody
    jsonResponse
    (params getType)
  return . fmap (filterByDate startDay endDay . f) $ responseBody resp
  where
    params StockGet { .. } =
      (  ("function" =: (sgFunc))
      <> ("symbol" =: symbol)
      <> ("outputsize" =: ("full" :: T.Text))
      <> ("datatype" =: ("json" :: T.Text))
      <> ("apikey" =: cApiKey cfg)
      )
    params CryptoGet { .. } =
      (  ("function" =: (cgFunc))
      <> ("symbol" =: symbol)
      <> ("market" =: cgMarket)
      <> ("apikey" =: cApiKey cfg)
      )

getPricesAtFrequency
  :: PriceDataFrequency
  -> AlphaVantageConfig
  -> T.Text
  -> Day
  -> Day
  -> IO (AlphaVantageResponse [(Day, Prices)])
getPricesAtFrequency freq =
  getByFunctionAndFilter (StockGet $ funcToUse freq) fromPriceList
  where
    funcToUse :: PriceDataFrequency -> T.Text
    funcToUse Daily   = "TIME_SERIES_DAILY" :: T.Text
    funcToUse Weekly  = "TIME_SERIES_WEEKLY" :: T.Text
    funcToUse Monthly = "TIME_SERIES_MONTHLY" :: T.Text

getCryptoPricesAtFrequency
  :: PriceDataFrequency
  -> AlphaVantageConfig
  -> T.Text
  -> Day
  -> Day
  -> IO (AlphaVantageResponse [(Day, CryptoPrices)])
getCryptoPricesAtFrequency freq =
  getByFunctionAndFilter (CryptoGet (funcToUse freq) "USD") fromCryptoPriceList
  where
    funcToUse :: PriceDataFrequency -> T.Text
    funcToUse Daily   = "DIGITAL_CURRENCY_DAILY" :: T.Text
    funcToUse Weekly  = "DIGITAL_CURRENCY_WEEKLY" :: T.Text
    funcToUse Monthly = "DIGITAL_CURRENCY_MONTHLY" :: T.Text