{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module StockPrices.Db where

import Control.Monad ( unless )
import Data.Decimal ( Decimal, realFracToDecimal )
import qualified Data.Text as T
import Data.Time ( Day, defaultTimeLocale, parseTimeOrError, showGregorian )
import Database.SQLite.Simple ( Connection
                              , Only (..)
                              , SQLData (..)
                              , close
                              , execute_
                              , executeMany
                              , open
                              , query
                              )
import Database.SQLite.Simple.FromRow ( FromRow (..), RowParser, field )
import Database.SQLite.Simple.ToRow ( ToRow (..) )
import System.Directory ( doesFileExist )
import System.FilePath ( FilePath, (</>) )

import StockPrices.Types

dbFilePath :: FilePath -> FilePath
dbFilePath baseDir = baseDir </> "commodity_prices.dat"

initDbConn :: FilePath -> IO Connection
initDbConn baseDir = do
  let fpath = dbFilePath baseDir
  dbFileExists <- doesFileExist fpath
  conn <- open fpath
  unless dbFileExists $ reseedDb conn
  pure conn

closeDbConn = close

reseedDb :: Connection -> IO ()
reseedDb conn = do
  execute_
    conn
    "CREATE TABLE prices (\
    \   id INT PRIMARY KEY AUTOINCREMENT,\
    \   ticker TEXT,\
    \   date TEXT,\
    \   open INT,\
    \   close INT,\
    \   low INT,\
    \   high INT,\
    \   volume INT,\
    \   crypto_market_cap INT,\
    \)"

instance ToRow GenericPrice where
  toRow :: GenericPrice -> [SQLData]
  toRow (Stock Prices {..}) =
    [ SQLInteger $ fromIntegral $ round (pOpen * 100)
    , SQLInteger $ fromIntegral $ round (pClose * 100)
    , SQLInteger $ fromIntegral $ round (pLow * 100)
    , SQLInteger $ fromIntegral $ round (pHigh * 100)
    , SQLInteger $ fromIntegral pVolume
    , SQLInteger (-1)
    ]
  toRow (Crypto CryptoPrices {..}) =
    [ SQLInteger $ fromIntegral $ round (cpOpen * 100)
    , SQLInteger $ fromIntegral $ round (cpClose * 100)
    , SQLInteger $ fromIntegral $ round (cpLow * 100)
    , SQLInteger $ fromIntegral $ round (cpHigh * 100)
    , SQLInteger 0
    , SQLInteger $ fromIntegral $ round (cpMarketCap * 100)
    ]

instance FromRow GenericPrice where
  fromRow :: RowParser GenericPrice
  fromRow = fmap f (fromRow :: RowParser (Int, Int, Int, Int, Int, Int))
    where
      f :: (Int, Int, Int, Int, Int, Int) -> GenericPrice
      f (open, close, low, high, volume, cap)
        | cap <= 0 = Stock $ Prices
            { pOpen   = convertIntToDecimal open
            , pClose  = convertIntToDecimal close
            , pLow    = convertIntToDecimal low
            , pHigh   = convertIntToDecimal high
            , pVolume = fromIntegral volume
            }
        | otherwise = Crypto $ CryptoPrices
            { cpOpen      = convertIntToDecimal open
            , cpClose     = convertIntToDecimal close
            , cpLow       = convertIntToDecimal low
            , cpHigh      = convertIntToDecimal high
            , cpMarketCap = convertIntToDecimal cap
            }
      convertIntToDecimal :: Int -> Decimal
      convertIntToDecimal i = realFracToDecimal 2 ((fromIntegral i) / 100.00)

data PriceRow = PriceRow
  { prTicker :: T.Text
  , prDate :: Day
  , prPrice :: GenericPrice
  }

instance FromRow PriceRow where
  fromRow :: RowParser PriceRow
  fromRow =
    PriceRow
      <$> field
      <*> (getDate <$> field)
      <*> fromRow
    where
      getDate :: T.Text -> Day
      getDate d = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" (T.unpack d)

instance ToRow PriceRow where
  toRow :: PriceRow -> [SQLData]
  toRow PriceRow {..} =
    [ SQLText prTicker
    , SQLText $ T.pack $ showGregorian prDate
    ] ++ (toRow prPrice)

insertPriceRows :: Connection -> [PriceRow] -> IO ()
insertPriceRows =
  flip
    executeMany
    "INSERT OR REPLACE INTO prices (ticker, date, open, close, low, high, \
    \volume, crypto_market_cap) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"

getPriceRows :: Connection -> T.Text -> Day -> Day -> IO [PriceRow]
getPriceRows conn ticker startDate endDate =
  query conn
    "SELECT ticker, date, open, close, low, high, volume, crypto_market_cap \
    \FROM prices WHERE ticker = ? AND date >= ? AND date <= ? ORDER BY date"
    (ticker, showGregorian startDate, showGregorian endDate)
