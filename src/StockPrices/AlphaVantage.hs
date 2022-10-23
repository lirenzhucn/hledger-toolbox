{-# LANGUAGE RecordWildCards #-}

module StockPrices.AlphaVantage where

import Data.Aeson ( FromJSON (..) )
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time ( Day )
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

import StockPrices.Types  ( AlphaVantageConfig (..)
                          , AlphaVantageResponse
                          , CryptoPriceList (..)
                          , CryptoPrices
                          , PriceDataFrequency (..)
                          , PriceList (..)
                          , Prices
                          )

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
