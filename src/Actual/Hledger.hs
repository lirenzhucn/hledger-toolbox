{-# LANGUAGE RecordWildCards #-}

module Actual.Hledger where

import Actual.Types (AppSettings (..), Transaction (..))
import Data.Decimal (DecimalRaw (Decimal))
import Data.Map (fromList)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
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
    amountstyle,
    mkPos,
  )
import Hledger.Data.Types (MixedAmount (..), MixedAmountKey (..))
import qualified Hledger as H (Transaction (..))

data MakeJournalConfig = MakeJournalConfig
  { cLastRead               :: POSIXTime
  , cAccountMapper          :: Text -> Text
  , cPayeeMapper            :: Maybe Text -> Maybe Text
  , cTransferAccount        :: Text
  , cStartingBalanceAccount :: Text
  }

buildConfig :: AppSettings -> POSIXTime -> MakeJournalConfig
buildConfig settings t =
  MakeJournalConfig
    { cLastRead               = t
    , cAccountMapper          = \acc -> fromMaybe acc (M.lookup acc (account_map settings))
    , cPayeeMapper            = \mp  -> mp >>= \p -> M.lookup p (payee_map settings)
    , cTransferAccount        = transfer_account settings
    , cStartingBalanceAccount = starting_balance_account settings
    }

makeJournal :: MakeJournalConfig -> [Transaction] -> Journal
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
      jdeclaredtags = [],
      jdeclaredaccounttags = M.empty,
      jaccounttypes = M.empty,
      jglobalcommoditystyles = M.empty,
      jcommodities = M.empty,
      jinferredcommodities = M.empty,
      jpricedirectives = [],
      jinferredmarketprices = [],
      jtxnmodifiers = [],
      jperiodictxns = [],
      jtxns = map (makeTransaction config) ts,
      jfinalcommentlines = "",
      jfiles = [],
      jlastreadtime = cLastRead config
    }

makeTransaction :: MakeJournalConfig -> Transaction -> H.Transaction
makeTransaction MakeJournalConfig {..} Transaction {..} =
  H.Transaction
    { H.tindex = 0,
      H.tprecedingcomment = "",
      H.tsourcepos = defaultSourcePosPair,
      H.tdate = txDate,
      H.tdate2 = Nothing,
      H.tstatus = if txCleared then Cleared else Pending,
      H.tcode = "",
      H.tdescription = mkDesc txPayeeName txNotes,
      H.tcomment = "",
      H.ttags = [],
      H.tpostings =
        [ Posting
            { pdate = Nothing,
              pdate2 = Nothing,
              pstatus = Unmarked,
              paccount = cAccountMapper txAccountName,
              pamount = Mixed (fromList [(MixedAmountKeyNoPrice "$", transAmt)]),
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
              pamount = Mixed (fromList [(MixedAmountKeyNoPrice "$", negTransAmt)]),
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
    negTransAmt = transAmt {aquantity = -(aquantity transAmt)}
    transAmt =
      Amount
        { acommodity = "$",
          aquantity  = Decimal 2 (toInteger txAmount),
          astyle     = amountstyle {asprecision = Precision 2},
          aprice     = Nothing
        }
    defaultSourcePosPair =
      (SourcePos "" (mkPos 1) (mkPos 1), SourcePos "" (mkPos 2) (mkPos 1))
    --
    mkDesc (Just payee) (Just notes) = payee <> " | " <> notes
    mkDesc Nothing      (Just notes) = notes
    mkDesc (Just payee) Nothing      = payee
    mkDesc Nothing      Nothing      = ""
    --
    mkAccount2
      | txStartingBalance                         = cStartingBalanceAccount
      | Just tx <- txTransferId, not (T.null tx)  = cTransferAccount
      | Just acc <- cPayeeMapper txPayeeName      = acc
      | txIsIncomeCat                             =
          "revenues:income:" <> fromMaybe "Unknown Payee" txPayeeName
      | Just grp <- txCategoryGroup, Just cat <- txCategoryName =
          "expenses:" <> grp <> ":" <> cat
      | Just cat <- txCategoryName               = "expenses:Uncategorized:" <> cat
      | otherwise                                = "expenses:Uncategorized"
