module Hledger.Utils.Render (renderJournal) where

import Data.Text.Lazy as TL (Text)
import Data.Text.Lazy.Builder as TB (fromText, toLazyText)
import Hledger
  ( Journal,
    Posting (..),
    Transaction (..),
    defreportspec,
    entriesReport,
    originalPosting,
    showTransaction,
  )

renderJournal :: Journal -> TL.Text
renderJournal j = render $ entriesReport defreportspec j
  where
    render = TB.toLazyText . foldMap (TB.fromText . showTransaction . originalTransaction)

originalTransaction :: Transaction -> Transaction
originalTransaction t = t {tpostings = map originalPostingPreservingAccount $ tpostings t}

originalPostingPreservingAccount :: Posting -> Posting
originalPostingPreservingAccount p =
  orig
    { paccount = paccount p,
      pamount = pamount $ if isGenerated then p else orig
    }
  where
    orig = originalPosting p
    isGenerated = "generated-posting" `elem` map fst (ptags p)
