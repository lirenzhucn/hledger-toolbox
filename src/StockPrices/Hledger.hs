{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module StockPrices.Hledger where

import Control.Concurrent ( threadDelay )
import Control.Exception ( SomeException
                         , displayException
                         , try
                         )
import Data.Bifunctor ( second )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Decimal ( Decimal, roundTo )
import Data.Default ( Default (..) )
import qualified Data.List as L
import Data.List.Split ( chunksOf )
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Maybe ( catMaybes )
import Data.Text.Encoding ( encodeUtf8 )
import Data.Time  ( Day
                  , UTCTime (utctDay)
                  , defaultTimeLocale
                  , formatTime
                  , fromGregorian
                  , getCurrentTime
                  , toGregorian
                  )
import Hledger
import Hledger.Data.Types ( MixedAmount (Mixed) )
import Hledger.Reports.BalanceReport ( balanceReport, BalanceReport )
import Hledger.Reports.ReportOptions ( ReportSpec(..) )
import Safe.Foldable ( maximumMay, minimumMay )
import System.IO ( hPutStrLn, stderr )

import StockPrices.Types  ( AlphaVantageConfig
                          , AlphaVantageRequest (..)
                          , AlphaVantageResponse (..)
                          , CryptoPrices (..)
                          , GenericPrice (..)
                          , PriceDataFrequency (..)
                          , Prices (..)
                          )
import StockPrices.AlphaVantage ( getPricesAtFrequency
                                , getCryptoPricesAtFrequency
                                )

-- Given a list of commodities to exclude and a journal file, return the
-- commodities in the journal and the minimum/maximum days from the journal
getCommoditiesAndDateRange :: [T.Text] -> FilePath -> IO [(CommoditySymbol, Day, Day)]
getCommoditiesAndDateRange excluded journalPath = do
  journal <- fmap (either error id) . runExceptT $ readJournalFile
    definputopts
    journalPath
  currentTime <- getCurrentTime
  let commodities =  L.sort $ L.nub $
        filter (`notElem` excluded)
          $ M.keys (jcommodities journal)
          <> M.keys (jinferredcommodities journal)
      res = map
        ( (adjustMaxDate currentTime journal)
        . (getDatesForCommodity currentTime (jtxns journal)))
        commodities
  return res
  where
    adjustMaxDate :: UTCTime -> Journal -> (CommoditySymbol, Day, Day) -> (CommoditySymbol, Day, Day)
    adjustMaxDate currentTime j (c, sd, ed) = case (commodityBalance j c) > 0 of
        True -> (c, sd, max ed (utctDay currentTime))
        False -> (c, sd, ed)
    commodityBalance :: Journal -> CommoditySymbol -> Quantity
    commodityBalance j c =
      let r = balanceReport defaultreportspec j
      in f r c
    f :: BalanceReport -> CommoditySymbol -> Quantity
    f (_, Mixed m) c = foldl (\q1 a2 -> q1 + (aquantity a2)) 0 $
      filter (\a -> acommodity a == c) $ [ a | (_, a) <- M.toList m ]
    defaultreportspec :: ReportSpec
    defaultreportspec = ReportSpec
      { _rsReportOpts = def
      , _rsDay        = nulldate
      , _rsQuery      = Any
      , _rsQueryOpts  = []
      }

commoditiesOfMixedAmount :: MixedAmount -> [CommoditySymbol]
commoditiesOfMixedAmount (Mixed m) = [ acommodity a | (_, a) <- M.toList m ]

commoditiesOfTransaction :: Transaction -> [CommoditySymbol]
commoditiesOfTransaction t = foldl (++) [] $ map (commoditiesOfMixedAmount . pamount) (tpostings t)

getDatesForCommodity :: UTCTime -> [Transaction] -> CommoditySymbol -> (CommoditySymbol, Day, Day)
getDatesForCommodity currentTime txns commodity = (commodity, minDate, maxDate)
  where
    currentYear = (\(y, _, _) -> y) $ toGregorian $ utctDay currentTime
    dates = map tdate $ filter (\t -> elem commodity (commoditiesOfTransaction t)) txns
    minDate = case minimumMay dates of
      Just d -> d
      Nothing -> fromGregorian currentYear 1 1
    maxDate = case maximumMay dates of
      Just d -> d
      Nothing -> utctDay currentTime

-- Fetch the prices for the commodities from the AlphaVantage API
-- limiting the returned prices between the given days
fetchPrices
  :: AlphaVantageConfig
  -> PriceDataFrequency
  -> [(CommoditySymbol, Day, Day)]
  -> [T.Text]
  -> Bool
  -> IO [(CommoditySymbol, [(Day, GenericPrice)])]
fetchPrices cfg freq comTbl cryptos rateLimit = do
  let (stockTable, cryptoTable) = L.partition (\(c, _, _) -> notElem c cryptos) comTbl
      genericAction = map FetchStock stockTable <> map FetchCrypto cryptoTable
  if rateLimit
    then fmap catMaybes $ rateLimitActions $ map fetch genericAction
    else catMaybes <$> mapM fetch genericAction
  where
    fetch :: AlphaVantageRequest -> IO (Maybe (CommoditySymbol, [(Day, GenericPrice)]))
    fetch req = do
      (symbol, label, resp) <- case req of
        FetchStock (symbol, start, end) ->
          (symbol, "Stock", )
            <$> try
              (fmap (map (second Stock))
              <$> getPricesAtFrequency freq cfg symbol start end
              )
        FetchCrypto (symbol, start, end) ->
          (symbol, "Cryptocurrency", )
            <$> try
              (fmap (map (second Crypto))
              <$> getCryptoPricesAtFrequency freq cfg symbol start end
              )
      case resp of
        Left (e :: SomeException) -> do
          logError
            $ "Error fetching prices for "
            <> label
            <> "  `"
            <> T.unpack symbol
            <> "`:\n\t"
            ++ displayException e
            ++ "\n"
          return Nothing
        Right (ApiError note) -> do
          logError
            $ "Error fetching prices for "
            <> label
            <> "  `"
            <> T.unpack symbol
            <> "`:\n\t"
            <> T.unpack note
            <> "\n"
          return Nothing
        Right (ApiResponse prices) -> return $ Just (symbol, prices)

    logError :: String -> IO ()
    logError = hPutStrLn stderr

rateLimitActions :: [IO a] -> IO [a]
rateLimitActions a = case chunksOf 5 a of
  [first] -> sequence first
  first:rest -> do
    rest_ <- concat <$> mapM runAndDelay rest
    first_ <- sequence first
    return $ first_ ++ rest_
  [] -> return []
  where
    runAndDelay actions = do
      results <- sequence actions
      putStrLn "waiting 60 seconds to respect API rate limit"
      threadDelay (60 * 1_000_000)
      return results

makePriceDirectives :: [(CommoditySymbol, [(Day, GenericPrice)])] -> LBS.ByteString
makePriceDirectives = (<> "\n") . LBS.intercalate "\n\n" . map makeDirectives
  where
    makeDirectives :: (CommoditySymbol, [(Day, GenericPrice)]) -> LBS.ByteString
    makeDirectives (symbol, prices) =
      LBS.intercalate "\n"
        $ ("; " <> LBS.fromStrict (encodeUtf8 symbol))
        : map (makeDirective symbol) prices
    makeDirective :: CommoditySymbol -> (Day, GenericPrice) -> LBS.ByteString
    makeDirective symbol (day, prices) =
      LBS.intercalate
        " "
        [ "P"
        , LC.pack $ formatTime defaultTimeLocale "%F" day
        , LBS.fromStrict $ encodeUtf8 symbol
        , "$" <> LC.pack (show $ getClosePrice prices)
        ]
    getClosePrice :: GenericPrice -> Decimal
    getClosePrice = \case
      Stock Prices { pClose } -> roundTo 2 pClose
      Crypto CryptoPrices { cpClose } -> roundTo 2 cpClose
