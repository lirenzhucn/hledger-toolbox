{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.Catch (finally)
import qualified Data.Text.IO as TIO
import Ynab
import Ynab.Db
import Ynab.Types
import Ynab.Req

work :: AppSettings -> IO ()
work settings = do
    env <- initEnv settings
    finally (runYnabApp (pure ()) env) (closeDbConn $ dbConn env)

main :: IO ()
main = do
    content <- TIO.readFile "./.secrets/ynab_api_import_settings.json"
    let maybeSettings = (decodeFromJSON content :: Maybe AppSettings)
    case maybeSettings of
        Nothing       -> putStrLn "Failed to parse settings"
        Just settings -> work settings
