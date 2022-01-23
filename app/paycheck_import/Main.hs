{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text.IO as TIO
import Paycheck (balanceOrWarnApp, runPaycheckApp, parsePaychecksApp, printJournalApp)
import Paycheck.Types (AppSettings)
import Utils (decodeFromJSON)

import Params (Params (..), cliParser)

work :: FilePath -> FilePath -> AppSettings -> IO ()
work inputDir _ settings = do
  runPaycheckApp doWork settings
  where
    doWork = parsePaychecksApp inputDir >>= balanceOrWarnApp >>= printJournalApp

main :: IO ()
main = do
  Params {..} <- cliParser
  content <- TIO.readFile settingsFile
  let maybeSettings = (decodeFromJSON content :: Maybe AppSettings)
  case maybeSettings of
    Nothing -> putStrLn "Failed to parse settings"
    Just settings -> work inputFile outputFile settings
