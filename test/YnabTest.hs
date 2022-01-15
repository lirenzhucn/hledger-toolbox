module Main (main) where

import Data.Aeson (FromJSON, decode)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Test.Tasty
import Test.Tasty.HUnit
import Ynab.Req
import Ynab.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [dataTypeJsonTests]

dataTypeJsonTests =
  testGroup
    "Data type JSON tests"
    [ testCase "CurrencyFormat from JSON" $ decodeFromJSON actualCFJson @?= expectedCF,
      testCase "ListDataField from JSON" $
        (decodeFromJSON actualLDFJson :: Maybe (ListDataField Payee)) @?= expectedLDF,
      testCase "ListDataField from JSON Failed 1" $
        (decodeFromJSON actualFailedLDFJson1 :: Maybe (ListDataField Payee)) @?= Nothing,
      testCase "ListDataField from JSON Failed 2" $
        (decodeFromJSON actualFailedLDFJson2 :: Maybe (ListDataField Payee)) @?= Nothing,
      testCase "ListDataField from JSON Failed 3" $
        (decodeFromJSON actualFailedLDFJson3 :: Maybe (ListDataField Payee)) @?= Nothing,
      testCase "Account from JSON" $ decodeFromJSON actualAccJson @?= expectedAcc,
      testCase "Budget from JSON" $ decodeFromJSON actualBudgetJson @?= expectedBudget,
      testCase "Payee from JSON" $ decodeFromJSON actualPayeeJson @?= expectedPayee,
      testCase "Payee Payload from JSON" $
        decodeFromJSON actualPayeePayloadJson
          @?= expectedPayeePayload
    ]
  where
    actualPayeePayloadJson =
      "{\
      \  \"data\": {\
      \    \"payees\": [\
      \      {\
      \        \"id\": \"string\",\
      \        \"name\": \"string\",\
      \        \"transfer_account_id\": \"string\",\
      \        \"deleted\": true\
      \      }\
      \    ],\
      \    \"server_knowledge\": 0\
      \  }\
      \}"
    expectedPayeePayload =
      Just $
        PayloadWrapper
          { dataField =
              ListDataField
                "payees"
                [ Payee
                    { payeeId = "string",
                      payeeName = "string",
                      payeeTransferAccountId = Just "string",
                      payeeDeleted = True
                    }
                ]
                (Just 0)
          }
    actualCFJson =
      "{\
      \    \"iso_code\": \"\",\
      \    \"example_format\": \"$1,000.00\",\
      \    \"decimal_digits\": 2,\
      \    \"decimal_separator\": \".\",\
      \    \"symbol_first\": true,\
      \    \"group_separator\": \",\",\
      \    \"currency_symbol\": \"$\",\
      \    \"display_symbol\": true\
      \}"
    expectedCF =
      Just
        CurrencyFormat
          { iso_code = "",
            example_format = "$1,000.00",
            decimal_digits = 2,
            decimal_separator = ".",
            symbol_first = True,
            group_separator = ",",
            currency_symbol = "$",
            display_symbol = True
          }
    actualLDFJson =
      "{\"payees\": [\
      \    {\
      \        \"id\": \"123\",\
      \        \"name\": \"Amazon\",\
      \        \"deleted\": false\
      \    },\
      \    {\
      \        \"id\": \"456\",\
      \        \"name\": \"Costco\",\
      \        \"deleted\": false\
      \    }\
      \]}"
    expectedLDF =
      Just $
        ListDataField
          "payees"
          [ Payee
              { payeeId = "123",
                payeeName = "Amazon",
                payeeTransferAccountId = Nothing,
                payeeDeleted = False
              },
            Payee
              { payeeId = "456",
                payeeName = "Costco",
                payeeTransferAccountId = Nothing,
                payeeDeleted = False
              }
          ]
          Nothing
    actualFailedLDFJson1 = "{\"field1\": [\"value1\"], \"field2\": [\"value2\"]}"
    actualFailedLDFJson2 = "{\"field\": 1}"
    -- payload key and data type mismatch
    actualFailedLDFJson3 =
      "{\"budgets\": [\
      \    {\"id\": \"123\", \"name\": \"Amazon\", \"deleted\": false}\
      \]}"
    actualAccJson =
      "{\
      \    \"balance\": -81699740,\
      \    \"cleared_balance\": -81699740,\
      \    \"closed\": false,\
      \    \"deleted\": false,\
      \    \"direct_import_in_error\": false,\
      \    \"direct_import_linked\": false,\
      \    \"id\": \"81d688b3-88d0-4663-ad8b-0446f0dbdb12\",\
      \    \"name\": \"ZHU Family Vault\",\
      \    \"note\": \"#type=long_term_liability\",\
      \    \"on_budget\": false,\
      \    \"transfer_payee_id\": \"76a2b19e-da78-4f7a-ba10-64fa67ca9eae\",\
      \    \"type\": \"otherLiability\",\
      \    \"uncleared_balance\": 0\
      \}"
    expectedAcc =
      Just
        Account
          { accountBalance = -81699740,
            accountClearedBalance = -81699740,
            accountClosed = False,
            accountDeleted = False,
            accountDirectImportInError = False,
            accountDirectImportLinked = False,
            accountId = "81d688b3-88d0-4663-ad8b-0446f0dbdb12",
            accountName = "ZHU Family Vault",
            accountNote = Just "#type=long_term_liability",
            accountOnBudget = False,
            accountTransferPayeeId = "76a2b19e-da78-4f7a-ba10-64fa67ca9eae",
            accountType = "otherLiability",
            accountUnclearedBalance = 0
          }
    actualBudgetJson =
      "{\
      \    \"id\": \"string\",\
      \    \"name\": \"string\",\
      \    \"last_modified_on\": \"2022-01-06T02:56:18.374Z\",\
      \    \"first_month\": \"string\",\
      \    \"last_month\": \"string\",\
      \    \"date_format\": {\
      \        \"format\": \"string\"\
      \    },\
      \    \"currency_format\": {\
      \        \"iso_code\": \"string\",\
      \        \"example_format\": \"string\",\
      \        \"decimal_digits\": 0,\
      \        \"decimal_separator\": \"string\",\
      \        \"symbol_first\": true,\
      \        \"group_separator\": \"string\",\
      \        \"currency_symbol\": \"string\",\
      \        \"display_symbol\": true\
      \    },\
      \    \"accounts\": [\
      \        {\
      \            \"id\": \"string\",\
      \            \"name\": \"string\",\
      \            \"type\": \"checking\",\
      \            \"on_budget\": true,\
      \            \"closed\": true,\
      \            \"note\": \"string\",\
      \            \"balance\": 0,\
      \            \"cleared_balance\": 0,\
      \            \"uncleared_balance\": 0,\
      \            \"transfer_payee_id\": \"string\",\
      \            \"direct_import_linked\": true,\
      \            \"direct_import_in_error\": true,\
      \            \"deleted\": true\
      \        }\
      \    ]\
      \}"
    expectedBudget =
      Just
        Budget
          { budgetId_ = "string",
            budgetName = "string",
            budgetLastModifiedOn = "2022-01-06T02:56:18.374Z",
            budgetFirstMonth = "string",
            budgetLastMonth = "string",
            budgetDateFormat = DateFormat {format = "string"},
            budgetCurrencyFormat =
              CurrencyFormat
                { iso_code = "string",
                  example_format = "string",
                  decimal_digits = 0,
                  decimal_separator = "string",
                  symbol_first = True,
                  group_separator = "string",
                  currency_symbol = "string",
                  display_symbol = True
                },
            budgetAccounts =
              Just
                [ Account
                    { accountId = "string",
                      accountName = "string",
                      accountType = "checking",
                      accountOnBudget = True,
                      accountClosed = True,
                      accountNote = Just "string",
                      accountBalance = 0,
                      accountClearedBalance = 0,
                      accountUnclearedBalance = 0,
                      accountTransferPayeeId = "string",
                      accountDirectImportLinked = True,
                      accountDirectImportInError = True,
                      accountDeleted = True
                    }
                ]
          }
    actualPayeeJson =
      "{\
      \    \"deleted\": false,\
      \    \"id\": \"6662c233-c6f6-4938-b4e0-4223f93d62df\",\
      \    \"name\": \"Transfer : LZ PRI HOL\",\
      \    \"transfer_account_id\": \"3e315021-8204-446b-b133-6c3c6c359719\"\
      \}"
    expectedPayee =
      Just
        Payee
          { payeeId = "6662c233-c6f6-4938-b4e0-4223f93d62df",
            payeeName = "Transfer : LZ PRI HOL",
            payeeDeleted = False,
            payeeTransferAccountId = Just "3e315021-8204-446b-b133-6c3c6c359719"
          }
