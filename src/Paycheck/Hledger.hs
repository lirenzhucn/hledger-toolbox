module Paycheck.Hledger where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Time (defaultTimeLocale, parseTimeOrError)
import Data.Time.Clock.POSIX (POSIXTime)
import Hledger
  ( Posting (..),
    Journal (..),
    SourcePos (..),
    Status (..),
    Transaction (..),
    mkPos
  )

import Paycheck.Parser (makeTransaction)
import Paycheck.Types

makeJournal :: POSIXTime -> AppSettings -> [Text] -> Journal
makeJournal lastRead settings contents =
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
      jtxns = mkTrans,
      jfinalcommentlines = "",
      jfiles = [],
      jlastreadtime = lastRead
    }
  where
    mkTrans = map (makeTransaction settings) contents
    defaultSourcePosPair =
      (SourcePos "" (mkPos 1) (mkPos 1), SourcePos "" (mkPos 2) (mkPos 1))
