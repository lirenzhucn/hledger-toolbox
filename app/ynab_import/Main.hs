module Main where

import qualified Data.Text.IO as TIO
import Ynab
import Ynab.Types
import Ynab.Req

work :: AppSettings -> IO ()
work settings = do
    let env = initEnv settings
    budget <- runYnabApp getBudget env
    putStrLn $ show budget

main :: IO ()
main = do
    content <- TIO.readFile "./.secrets/ynab_api_import_settings.json"
    let maybeSettings = (decodeFromJSON content :: Maybe AppSettings)
    case maybeSettings of
        Nothing -> putStrLn "Failed to parse settings"
        Just settings -> work settings
