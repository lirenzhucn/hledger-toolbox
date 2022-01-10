{-# LANGUAGE ScopedTypeVariables #-}

module Ynab.Req (getBudget, ListDataField (..)) where

import Control.Monad (when)
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Key (toString)
import Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Exts (IsList (toList))
import GHC.Generics (Generic)
import Network.HTTP.Req
import Text.URI (URI, mkURI)
import Ynab.STExcept (YnabAPIException (..), rethrowReqException)
import Ynab.Types

newtype PayloadWrapper p = PayloadWrapper {dataField :: p}
  deriving (Eq, Show, Generic)

instance (FromJSON p) => FromJSON (PayloadWrapper p) where
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = fieldNameMap
        }
    where
      fieldNameMap :: String -> String
      fieldNameMap "dataField" = "data"
      fieldNameMap s = s

data ListDataField a = ListDataField
  {fieldName :: String, fieldValue :: [a]}
  deriving (Eq, Show)

instance (FromJSON a, PayloadItems a) => FromJSON (ListDataField a) where
  parseJSON = withObject "ListDataField" $ \v ->
    parseItemList $
      filter (\(k, v) -> toString k `elem` validKeys) $ toList v
    where
      -- NOTE: Need ScopedTypeVariable extension to work!
      parseItemList :: [(Key, Value)] -> Parser (ListDataField a)
      parseItemList ((k, v):_) = ListDataField (toString k) <$> (parseJSON v :: Parser [a])
      parseItemList xs = fail "Object should have at least one matched key-value pair"
      validKeys :: [String]
      validKeys = ["budgets", "accounts", "payees"]

-- build authentication header
buildAuthHeader :: Token -> Option schema
buildAuthHeader t = header "Authorization" $ "Bearer " <> encodeUtf8 t

type BudgetsDataField = ListDataField Budget

-- build get request
buildWrappedGETRequest ::
  (MonadHttp m, FromJSON d) =>
  Url scheme ->
  Option scheme ->
  m (JsonResponse (PayloadWrapper d))
buildWrappedGETRequest ep =
  req
    GET
    ep
    NoReqBody
    (jsonResponse :: Proxy (JsonResponse (PayloadWrapper d)))

getBudget :: AppSettings -> Url 'Https -> IO Budget
getBudget appSettings baseURL = do
  let apiSettings = ynab_api_settings appSettings
      ep = baseURL /: "budgets"
      request = buildWrappedGETRequest ep $ buildAuthHeader $ api_token apiSettings
  res <- responseBody <$> runReq defaultHttpConfig request
  let budgetsField = dataField res :: (ListDataField Budget)
  when (fieldName budgetsField /= "budgets") $ throwM (InvalidPayloadData "budgets")
  case validBudgets budgetsField of
    [] -> throwM (InvalidPayloadData "budgets")
    (x : _) -> pure x
  where
    validBudgets = filter (\b -> budget_name_ b == budget_name appSettings) . fieldValue

-- getItemList ::
--   YnabApiSettings ->
--   Url 'Https ->
--   Text ->
--   Text ->
--   Text ->
--   Maybe Text ->
--   IO ([a], Text)
-- getItemList settings baseURL budgetId itemTypeName payloadKey maybeSK = do
--   let ep = baseURL /: "budgets" /: budgetId /: itemTypeName
--       request = buildWrappedGETRequest ep $ reqParams $ api_token settings
--   res <- responseBody <$> runReq defaultHttpConfig request
--   let items = dataField res
--   where
--     reqParams token = case maybeSK of
--       Nothing -> buildAuthHeader token
--       Just sk -> mconcat ["last_knowledge_of_server" =: sk, buildAuthHeader token]
