module YnabDbTest where

import Data.List (sort)
import Database.SQLite.Simple (Connection, Only, close, fromOnly, open, query_)
import Test.Tasty
import Test.Tasty.HUnit
import Ynab.Db
import Ynab.Types

initTestDb :: IO Connection
initTestDb = do
  conn <- open ":memory:"
  reseedDb conn
  pure conn

closeTestDb :: Connection -> IO ()
closeTestDb = close

-- add an instance of Ord for Category
instance Ord Category where
  compare c1 c2 = (categoryId c1) `compare` (categoryId c2)

instance Ord TransactionDb where
  compare t1 t2 = (trdId t1) `compare` (trdId t2)

dbTests :: IO Connection -> TestTree
dbTests getConn =
  testGroup
    "Database tests"
    [ testCase "reseedDb should create tables" $ checkTablesInDb,
      testCase "reseedDb should clear the tables" $ checkTablesEmpty,
      testCase "server knowledge set/get round trip yields the same result" $
        checkSKRoundTrip,
      testCase "categories insert/get round trip yields the same result" $
        checkCategoriesRoundTrip,
      testCase "transactions insert/get round trip yileds the same result" $
        checkTransRoundTrip
    ]
  where
    checkTablesInDb = do
      conn <- getConn
      tables <- query_
        conn
        "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE \
        \'sqlite_%'" :: IO [Only String]
      (sort $ fmap fromOnly tables) @?=
        (sort ["server_knowledge", "accounts", "payees", "categories", "transactions"])
    checkTablesEmpty = do
      conn <- getConn
      nAccounts <- fmap
        (fromOnly . head)
        (query_ conn "SELECT count(*) FROM accounts" :: IO [Only Integer])
      nPayees <- fmap
        (fromOnly . head)
        (query_ conn "SELECT count(*) FROM payees" :: IO [Only Integer])
      nCategories <- fmap
        (fromOnly . head)
        (query_ conn "SELECT count(*) FROM categories" :: IO [Only Integer])
      nTransactions <- fmap
        (fromOnly . head)
        (query_ conn "SELECT count(*) FROM transactions" :: IO [Only Integer])
      nAccounts @?= 0
      nPayees @?= 0
      nCategories @?= 0
      nTransactions @?= 0
    checkSKRoundTrip = do
      conn <- getConn
      let sks1 = ServerKnowledgeSet
            { serverKnowledgeAccounts = Just "123",
              serverKnowledgePayees = Just "456",
              serverKnowledgeCategoryGroups = Just "789",
              serverKnowledgeTransactions = Just "357"
            }
          sks2 = ServerKnowledgeSet
            { serverKnowledgeAccounts = Nothing,
              serverKnowledgePayees = Nothing,
              serverKnowledgeCategoryGroups = Nothing,
              serverKnowledgeTransactions = Nothing
            }
      actualSks0 <- getServerKnowledgeFromDb conn
      actualSks0 @?= ServerKnowledgeSet Nothing Nothing Nothing Nothing
      setServerKnowledgeToDb conn sks1
      actualSks1 <- getServerKnowledgeFromDb conn
      actualSks1 @?= sks1
      setServerKnowledgeToDb conn sks2
      actualSks2 <- getServerKnowledgeFromDb conn
      actualSks2 @?= sks2
    checkCategoriesRoundTrip = do
      conn <- getConn
      actualCs0 <- getAllCategories conn
      actualCs0 @?= []
      let cs1 = [ Category "123" False "Cat-1" (Just "Cat-1 Note") "123" (Just "CG-1"),
                  Category "456" False "Cat-2" Nothing "123" (Just "CG-1"),
                  Category "789" False "Cat-3" Nothing "456" Nothing
                ]
          d12 = [ Category "123" True "Cat-1" (Just "Cat-1 Note") "123" (Just "CG-1"),
                  Category "246" False "Cat-4" Nothing "456" Nothing
                ]
          cs2 = [ Category "123" True "Cat-1" (Just "Cat-1 Note") "123" (Just "CG-1"),
                  Category "456" False "Cat-2" Nothing "123" (Just "CG-1"),
                  Category "789" False "Cat-3" Nothing "456" Nothing,
                  Category "246" False "Cat-4" Nothing "456" Nothing
                ]
      insertCategories conn cs1
      actualCs1 <- getAllCategories conn
      actualCs1 @?= cs1
      insertCategories conn d12
      actualCs2 <- getAllCategories conn
      (sort actualCs2) @?= (sort cs2)
    checkTransRoundTrip = do
      conn <- getConn
      actualTrans0 <- getAllTransactions conn
      actualTrans0 @?= []
      let ts1 = [ transactionDb {trdId = "123"},
                  transactionDb {trdId = "456"}
                ]
          d12 = [ transactionDb {trdId = "123", trdDeleted = True},
                  transactionDb {trdId = "789"}
                ]
          ts2 = [ transactionDb {trdId = "123", trdDeleted = True},
                  transactionDb {trdId = "456"},
                  transactionDb {trdId = "789"}
                ]
      insertTransactions conn ts1
      actualTrans1 <- getAllTransactions conn
      (sort actualTrans1) @?= (sort ts1)
      insertTransactions conn d12
      actualTrans2 <- getAllTransactions conn
      (sort actualTrans2) @?= (sort ts2)
