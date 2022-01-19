module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import YnabTypesJsonTest
import YnabDbTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ dataTypeJsonTests,
    transactionJsonTests,
    withResource initTestDb closeTestDb dbTests
  ]
