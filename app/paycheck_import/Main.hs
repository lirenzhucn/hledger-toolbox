{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Text.Lazy (toStrict)
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Hledger.Utils.Render (renderJournal)
import Paycheck.Types (AppSettings)
import Paycheck.Hledger (makeJournal)
import Utils (decodeFromJSON, readPdfOrTxt_)

import Params (Params (..), cliParser)

work :: FilePath -> FilePath -> AppSettings -> IO ()
work inputFile _ settings = do
    content <- readPdfOrTxt_ inputFile
    currTime <- getPOSIXTime
    let journal = makeJournal currTime settings content
    TIO.putStrLn $ toStrict $ renderJournal journal

main :: IO ()
main = do
    Params {..} <- cliParser
    content <- TIO.readFile settingsFile
    let maybeSettings = (decodeFromJSON content :: Maybe AppSettings)
    case maybeSettings of
        Nothing -> putStrLn "Failed to parse settings"
        Just settings -> work inputFile outputFile settings
