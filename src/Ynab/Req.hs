module Ynab.Req (getBudget) where

import Control.Monad.Catch
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Maybe (isNothing)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
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

newtype BudgetDataField = BudgetDataField {budgets :: [Budget]}
  deriving (Eq, Show, Generic, FromJSON)

-- build authentication header
buildAuthHeader :: Token -> Option schema
buildAuthHeader t = header "Authorization" $ "Bearer " <> encodeUtf8 t

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
  let BudgetDataField {budgets} = dataField res
  case filter (\b -> budget_name_ b == budget_name appSettings) budgets of
    [] -> throwM (InvalidPayloadData "budgets")
    (x : _) -> pure x
