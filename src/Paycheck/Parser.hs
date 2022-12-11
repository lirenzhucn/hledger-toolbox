{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Paycheck.Parser where

import Data.Decimal (DecimalRaw (Decimal))
import Data.Map (fromList)
import Data.Maybe (catMaybes)
import Data.Text (Text, replace, unpack)
import Data.Time (ParseTime, defaultTimeLocale, parseTimeOrError)
import Hledger
import Hledger.Read.Common (parsemixedamount')
import Text.Regex.PCRE (MatchResult (..), (=~))
import Text.Regex.PCRE.Text

import Paycheck.Types

amountExtractor :: Text -> Text -> Text -> Maybe MixedAmount
amountExtractor amountConfig patternConfig content =
  let matchAmountsT = mrSubList (content =~ patternConfig) in
    case matchAmountsT of
      [] -> Nothing
      xs -> Just $ mkAmount xs
  where
    mkAmount ms = parsemixedamount' $ unpack $
      (matchAmountFields !! 0) <> "$" <> (replace "," "" (matchAmountT ms))
    matchAmountT ms = ms !! ((read $ unpack (matchAmountFields !! 1) :: Int) - 1)
    matchAmountFields = mrSubList
      (amountConfig =~ ("([+-]{0,1})\\$(\\d)" :: Text) :: MatchResult Text)

extractPosting :: PostingConfig -> Text -> Maybe Posting
extractPosting PostingConfig {..} content =
  (amountExtractor pcAmount pcPattern content) >>=
    (\someAmount ->
      Just Posting
        { pdate = Nothing,
          pdate2 = Nothing,
          pstatus = Unmarked,
          paccount = pcAccount,
          pamount = someAmount,
          pcomment = "",
          ptype = RegularPosting,
          ptags = [],
          pbalanceassertion = Nothing,
          ptransaction = Nothing,
          poriginal = Nothing
        })

checkDateExtractor :: ParseTime t => Text -> Text -> Text -> t
checkDateExtractor pattern dateFormat content =
  let dateText = head $ mrSubList (content =~ pattern :: MatchResult Text) in
    parseTimeOrError True defaultTimeLocale (unpack dateFormat) (unpack dateText)

buildTransDesc :: GlobalConfig -> Text -> Text
buildTransDesc GlobalConfig {..} content =
  replace "${recipient_name}" recipientName $
    replace "${check_number}" checkNumber $
      replace "${employer_name}" employerName $
        replace "${check_date}" checkDate gcTransDesc
  where
    matchFirst pattern = head $ mrSubList (content =~ (pattern :: Text))
    recipientName = matchFirst gcRecipientName
    employerName = matchFirst gcEmployerName
    checkDate = matchFirst gcCheckDate
    checkNumber = matchFirst gcCheckNumber

makeTransaction :: AppSettings -> Text -> Transaction
makeTransaction AppSettings {..} content =
  Transaction
    { tindex = 0,
      tprecedingcomment = "",
      tsourcepos = defaultSourcePosPair,
      tdate = checkDateExtractor (gcCheckDate asGlobal) (gcDateFormat asGlobal) content,
      tdate2 = Nothing,
      tstatus = Cleared,
      tcode = "",
      tdescription = buildTransDesc asGlobal content,
      tcomment = "",
      ttags = [],
      tpostings = catMaybes $ map (flip extractPosting content) asPostings
    }
  where
    defaultSourcePosPair =
      (SourcePos "" (mkPos 1) (mkPos 1), SourcePos "" (mkPos 2) (mkPos 1))
