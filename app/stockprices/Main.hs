{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad ( forM_ )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Time ( Day, defaultTimeLocale, formatTime )
import Hledger ( CommoditySymbol )

import Config ( AppConfig (..)
              , Args (..)
              , cliParser
              , loadConfigFile
              , mergeArgsEnvCfg
              )
import StockPrices.AlphaVantage ( AlphaVantageConfig (..) )
import StockPrices.Hledger ( getCommoditiesAndDateRange
                           , fetchPrices
                           , makePriceDirectives
                           )
import Utils ( logError )

main :: IO ()
main = do
  configFile <- loadConfigFile
  args <- cliParser
  AppConfig {..} <- mergeArgsEnvCfg configFile args
  let cfg = AlphaVantageConfig $ T.pack $ apiKey
  comTbl <- getCommoditiesAndDateRange
    (T.pack <$> excludedCommodities )
    journalFile
  if not dryRun
    then do
      prices <- fetchPrices cfg comTbl cryptoCommodities rateLimit
      if null prices
        then logError "No prices were fetched"
        else LBS.writeFile outputFile $ makePriceDirectives prices
    else do
      forM_ comTbl putComTblItem
  where
    putComTblItem :: (CommoditySymbol, Day, Day) -> IO ()
    putComTblItem (symbol, start, end) =
      putStrLn $ "\t"
        <> T.unpack symbol
        <> " from "
        <> showDate start
        <> " to "
        <> showDate end
    showDate :: Day -> String
    showDate = formatTime defaultTimeLocale "%Y-%m-%d"
