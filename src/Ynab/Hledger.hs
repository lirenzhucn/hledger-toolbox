{-# LANGUAGE RecordWildCards #-}

module Ynab.Hledger where

import Data.Decimal (DecimalRaw (Decimal))
import Data.Map (fromList)
import qualified Data.Map.Strict as M (empty)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeOrError)
import Data.Time.Clock.POSIX (POSIXTime)
import Hledger
  ( Amount (..),
    AmountPrecision (Precision),
    AmountStyle (asprecision),
    Journal (..),
    Posting (..),
    PostingType (RegularPosting),
    SourcePos (..),
    Status (..),
    Transaction (..),
    amountstyle,
    mkPos,
    usd,
  )
import Hledger.Data.Types (MixedAmount (..), MixedAmountKey (..))
import Ynab.Types (TransactionDb (..))

data MakeJournalConfig = MakeJournalConfig
  { cLastRead :: POSIXTime,
    cAccountMapper :: Text -> Text,
    -- map payee name to a possible account2
    cPayeeMapper :: Maybe Text -> Maybe Text,
    -- map category name to category group name
    cCategoryMapper :: Maybe Text -> Text,
    cTransferAccount :: Text,
    cStartingBalanceAccount :: Text
  }

makeJournal :: MakeJournalConfig -> [TransactionDb] -> Journal
makeJournal config ts =
  Journal
    { jparsedefaultyear = Nothing,
      jparsedefaultcommodity = Nothing,
      jparsedecimalmark = Nothing,
      jparseparentaccounts = [],
      jparsealiases = [],
      jparsetimeclockentries = [],
      jincludefilestack = [],
      jdeclaredpayees = [],
      jdeclaredaccounts = [],
      jdeclaredaccounttypes = M.empty,
      jglobalcommoditystyles = M.empty,
      jcommodities = M.empty,
      jinferredcommodities = M.empty,
      jpricedirectives = [],
      jinferredmarketprices = [],
      jtxnmodifiers = [],
      jperiodictxns = [],
      jtxns = [makeTransaction config t | t <- ts, hasNoChildren t, not (trdDeleted t)],
      jfinalcommentlines = "",
      jfiles = [],
      jlastreadtime = cLastRead config
    }

hasNoChildren :: TransactionDb -> Bool
hasNoChildren t = null $ trdChildrenIds t

makeTransaction :: MakeJournalConfig -> TransactionDb -> Transaction
makeTransaction MakeJournalConfig {..} TransactionDb {..} =
  Transaction
    { tindex = 0,
      tprecedingcomment = "",
      tsourcepos = defaultSourcePosPair,
      tdate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" (unpack trdDate),
      tdate2 = Nothing,
      tstatus = mkStatus trdCleared,
      tcode = "",
      tdescription = mkDesc trdPayeeName trdMemo,
      tcomment = "",
      ttags = [],
      tpostings =
        [ Posting
            { pdate = Nothing,
              pdate2 = Nothing,
              pstatus = Unmarked,
              paccount = cAccountMapper trdAccountName,
              pamount = Mixed (fromList [(MixedAmountKeyNoPrice "$", transAmount)]),
              pcomment = "",
              ptype = RegularPosting,
              ptags = [],
              pbalanceassertion = Nothing,
              ptransaction = Nothing,
              poriginal = Nothing
            },
          Posting
            { pdate = Nothing,
              pdate2 = Nothing,
              pstatus = Unmarked,
              paccount = mkAccount2,
              pamount = Mixed (fromList [(MixedAmountKeyNoPrice "$", negTransAmount)]),
              pcomment = "",
              ptype = RegularPosting,
              ptags = [],
              pbalanceassertion = Nothing,
              ptransaction = Nothing,
              poriginal = Nothing
            }
        ]
    }
  where
    negTransAmount = transAmount {aquantity = - (aquantity transAmount)}
    transAmount =
      Amount
        { acommodity = "$",
          aquantity = Decimal 3 (toInteger trdAmount),
          astyle = amountstyle {asprecision = Precision 2},
          aprice = Nothing
        }
    defaultSourcePosPair =
      (SourcePos "" (mkPos 1) (mkPos 1), SourcePos "" (mkPos 2) (mkPos 1))
    --
    mkDesc (Just payee) (Just memo) = payee <> " | " <> memo
    mkDesc Nothing (Just memo) = memo
    mkDesc (Just payee) Nothing = payee
    mkDesc Nothing Nothing = ""
    --
    mkStatus "cleared" = Cleared
    mkStatus "reconciled" = Cleared
    mkStatus _ = Pending
    --
    isPayeeStartingBalance (Just s) = T.isPrefixOf "starting balance" (T.toLower s)
    isPayeeStartingBalance Nothing = False
    --
    isPayeeTransfer (Just s) = T.isPrefixOf "transfer" (T.toLower s)
    isPayeeTransfer Nothing = False
    --
    isCategoryInFlow (Just s) = T.toLower s == "inflow: ready to assign"
    isCategoryInFlow Nothing = False
    --
    mkAccount2
      | isPayeeStartingBalance trdPayeeName = cStartingBalanceAccount
      | isPayeeTransfer trdPayeeName = cTransferAccount
      | Just acc2 <- cPayeeMapper trdPayeeName = acc2 
      | isCategoryInFlow trdCategoryName =
        "revenues:income:" <> fromMaybe "Unknown Payee" trdPayeeName
      | otherwise =
        "expenses:"
          <> cCategoryMapper trdCategoryName
          <> ":"
          <> fromMaybe "Unknown Category:" trdCategoryName
