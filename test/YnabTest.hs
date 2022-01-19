module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import YnabTypesJsonTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [dataTypeJsonTests, transactionJsonTests]
