module Utils where

import Control.Monad (when)
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (encodeUtf8)
import System.Directory (removeFile)
import System.FilePath.Posix (dropExtension, takeExtension)
import System.Process (callProcess)

decodeFromJSON :: FromJSON a => Text -> Maybe a
decodeFromJSON = decode . fromStrict . encodeUtf8

readPdfOrTxt :: FilePath -> FilePath -> IO Text
readPdfOrTxt pdftotextBin fpath = do
  let txtPath = dropExtension fpath ++ (".txt" :: FilePath)
  when (takeExtension fpath /= ".txt") $
    callProcess pdftotextBin ["-layout", "-enc", "ASCII7", fpath]
  res <- TIO.readFile txtPath
  when (takeExtension fpath /= ".txt") $
    removeFile txtPath
  pure res

readPdfOrTxt_ :: FilePath -> IO Text
readPdfOrTxt_ = readPdfOrTxt "pdftotext"
