module Main where

import Config ( Args (..), cliParser, loadConfigFile, mergeArgsEnvCfg )

main :: IO ()
main = do
  configFile <- loadConfigFile
  args <- cliParser
  appConfig <- mergeArgsEnvCfg configFile args
  putStrLn $ show appConfig
