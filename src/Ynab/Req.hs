{-# LANGUAGE ScopedTypeVariables #-}

module Ynab.Req (getItemList, getBudget, ListDataField (..), PayloadWrapper (..)) where

import Control.Monad (when)
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Key (toString)
import qualified Data.Aeson.KeyMap as AKM
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
  { fieldName :: String,
    fieldValue :: [a],
    serverKnowledge :: Maybe Integer
  }
  deriving (Eq, Show)

instance (FromJSON a, PayloadItems a) => FromJSON (ListDataField a) where
  parseJSON = withObject "ListDataField" $ \v ->
    parseItemList (extractSK v) $
      filter (\(k, v) -> toString k `elem` validKeys) $ toList v
    where
      -- extract server knowledge payload (Maybe Integer) and wrap it in a Parser context
      extractSK :: Object -> Parser (Maybe Integer)
      extractSK v = do
        let maybePsk = fmap parseJSON (AKM.lookup "server_knowledge" v)
        case maybePsk of
          Nothing -> pure Nothing
          Just psk -> fmap Just psk
      -- NOTE: Need ScopedTypeVariable extension to work!
      parseItemList :: Parser (Maybe Integer) -> [(Key, Value)] -> Parser (ListDataField a)
      parseItemList sk ((k, v) : _) = do
        t <- parseJSON v :: Parser [a]
        let flags = fmap (`isKeyValid` toString k) t
        if and flags
          then -- then pure $ ListDataField (toString k) t Nothing
            fmap (ListDataField (toString k) t) sk
          else fail $ "Invalid payload key (" <> toString k <> ")"
      parseItemList _ _ = fail "Object should have at least one matched key-value pair"
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
    validBudgets = filter (\b -> budgetName b == budget_name appSettings) . fieldValue

getItemList ::
  forall a.
  (FromJSON a, PayloadItems a) =>
  YnabApiSettings ->
  Url 'Https ->
  Text ->
  Text ->
  Maybe Text ->
  IO ([a], Text)
getItemList settings baseURL budgetId itemTypeName maybeSK = do
  let ep = baseURL /: "budgets" /: budgetId /: itemTypeName
      request = buildWrappedGETRequest ep $ reqParams $ api_token settings
  res <- responseBody <$> runReq defaultHttpConfig request
  let listField = dataField res :: (ListDataField a)
  pure (fieldValue listField, "")
  where
    reqParams token = case maybeSK of
      Nothing -> buildAuthHeader token
      Just sk -> mconcat ["last_knowledge_of_server" =: sk, buildAuthHeader token]
