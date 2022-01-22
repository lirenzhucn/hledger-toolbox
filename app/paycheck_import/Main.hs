{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (mapM)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Hledger.Utils.Render (renderJournal)
import Paycheck.Types (AppSettings)
import Paycheck.Hledger (makeJournal)
import Utils (decodeFromJSON, readPdfOrTxt_)
import System.FilePath.Glob (compile, globDir1)

import Params (Params (..), cliParser)

work :: FilePath -> FilePath -> AppSettings -> IO ()
work inputDir _ settings = do
  currTime <- getPOSIXTime
  inputFiles <- globDir1 (compile "*.pdf") inputDir
  contents <- mapM readPdfOrTxt_ inputFiles
  let journal = makeJournal currTime settings contents
  TIO.putStrLn $ toStrict $ renderJournal journal

main :: IO ()
main = do
  Params {..} <- cliParser
  content <- TIO.readFile settingsFile
  let maybeSettings = (decodeFromJSON content :: Maybe AppSettings)
  case maybeSettings of
    Nothing -> putStrLn "Failed to parse settings"
    Just settings -> work inputFile outputFile settings
