module Main (main) where

import Data.Time ( fromGregorian )
import Test.Tasty
import Test.Tasty.HUnit

import StockPrices.AlphaVantage
import StockPrices.Hledger
import Utils

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ alphaVantageJsonTests
  , hledgerTests
  ]

alphaVantageJsonTests =
  testGroup
    "AlphaVantage JSON tests"
    [ testCase "Stock Price List" $
        decodeFromJSON actualStockPriceListJson0 @?= expectedStockPriceList0
    ]
  where
    actualStockPriceListJson0 =
      "{\
      \  \"Time Series (Daily)\": {\
      \    \"2022-10-14\": {\
      \       \"1. open\": \"121.8000\",\
      \       \"2. high\": \"122.5400\",\
      \       \"3. low\": \"119.8400\",\
      \       \"4. close\": \"120.0400\",\
      \       \"5. volume\": \"3763840\"\
      \    },\
      \    \"2022-10-13\": {\
      \      \"1. open\": \"116.1000\",\
      \      \"2. high\": \"122.1500\",\
      \      \"3. low\": \"115.5450\",\
      \      \"4. close\": \"121.7900\",\
      \      \"5. volume\": \"5837645\"\
      \    }\
      \  }\
      \}"
    expectedStockPriceList0 =
      Just $
        PriceList
          { fromPriceList = [
            ( (fromGregorian 2022 10 14)
              , Prices
                { pOpen = read "121.8000"
                , pHigh = read "122.5400"
                , pLow = read "119.8400"
                , pClose = read "120.0400"
                , pVolume = 3763840
                }
            ),
            ( (fromGregorian 2022 10 13)
              , Prices
                { pOpen = read "116.1000"
                , pHigh = read "122.1500"
                , pLow = read "115.5450"
                , pClose = read "121.7900"
                , pVolume = 5837645
                }
            )]
          }

hledgerTests =
  testGroup
    "Hledger related tests"
    [ testCase "Get date range of commodity" $ do
        res <- getCommoditiesAndDateRange ["$", "USD", "AUTO"] "./tests/data/sample.journal"
        res @?= [("AAPL", (fromGregorian 2009 1 1), (fromGregorian 2009 11 12))]
    ]
