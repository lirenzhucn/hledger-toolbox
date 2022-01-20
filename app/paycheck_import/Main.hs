module Main where

import qualified Data.Text.IO as TIO
import Paycheck.Types
import Utils (decodeFromJSON)

work :: AppSettings -> IO ()
work settings = do
    print settings

main :: IO ()
main = do
    content <- TIO.readFile $ "./.secrets/paycheck_import_settings.json"
    let maybeSettings = (decodeFromJSON content :: Maybe AppSettings)
    case maybeSettings of
        Nothing -> putStrLn "Failed to parse settings"
        Just settings -> work settings
