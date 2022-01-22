module Utils where

import Control.Monad (when)
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (encodeUtf8)
import System.FilePath.Posix (dropExtension, takeExtension)
import System.Process (callProcess)

decodeFromJSON :: FromJSON a => Text -> Maybe a
decodeFromJSON = decode . fromStrict . encodeUtf8

readPdfOrTxt :: FilePath -> FilePath -> IO Text
readPdfOrTxt pdftotextBin fpath = do
  when (takeExtension fpath /= ".txt") $
    callProcess pdftotextBin ["-layout", fpath]
  TIO.readFile $ dropExtension fpath ++ (".txt" :: FilePath)

readPdfOrTxt_ :: FilePath -> IO Text
readPdfOrTxt_ = readPdfOrTxt "pdftotext"
