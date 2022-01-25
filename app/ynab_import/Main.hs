module Main where

import Control.Monad.Catch (finally)
import Control.Monad.Reader (MonadIO (liftIO))
import qualified Data.Text.IO as TIO
import Utils (decodeFromJSON)
import Ynab
import Ynab.Db
import Ynab.Types

import Params (Params (..), cliParser)

work :: Params -> AppSettings -> IO ()
work params settings = do
  env <- initEnv (dbDir params) settings
  finally (runYnabApp doWork env) (closeDbConn $ dbConn env)
  where
    doWork
     | noPullData params = writeJournal (year params) (outputFile params)
     | otherwise = fetchData >> writeJournal (year params) (outputFile params)

main :: IO ()
main = do
  params <- cliParser
  content <- TIO.readFile $ settingsFile params
  let maybeSettings = (decodeFromJSON content :: Maybe AppSettings)
  case maybeSettings of
    Nothing -> putStrLn "Failed to parse settings"
    Just settings -> work params settings
