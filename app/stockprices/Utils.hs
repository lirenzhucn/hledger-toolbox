module Utils where

import System.IO ( hPutStrLn, stderr )

logError :: String -> IO ()
logError = hPutStrLn stderr . ("[ERROR] " <>)

logWarning :: String -> IO ()
logWarning = hPutStrLn stderr . ("[WARN] " <>)
