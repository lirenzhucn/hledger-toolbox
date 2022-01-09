{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ynab where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (runReaderT))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Req (Scheme (Https), Url, https, useHttpsURI, (/:))
import Text.URI (URI, mkURI)
import Ynab.Db
import Ynab.Req
import Ynab.Types

newtype YnabApp a = YnabApp
  {runApp :: ReaderT AppEnv IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader AppEnv
    )

runYnabApp :: YnabApp a -> AppEnv -> IO a
runYnabApp app = runReaderT (runApp app)

initEnv :: AppSettings -> IO AppEnv
initEnv settings = do
  let url = fromMaybe defaultURL maybeURL
  budget <- getBudget settings url
  conn <- initDbConn ".secrets/dbs" (budget_id budget)
  pure
    AppEnv
      { appSettings = settings,
        baseURL = url,
        dbConn = conn
      }
  where
    maybeURL = makeURL $ api_base_url $ ynab_api_settings settings
    defaultURL = https "api.youneedabudget.com" /: "v1"

makeURL :: Text -> Maybe (Url 'Https)
makeURL urlText =
  pure urlText >>= mkURI >>= useHttpsURI >>= (\(ep, _) -> Just ep)
