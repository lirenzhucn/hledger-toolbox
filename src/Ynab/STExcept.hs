module Ynab.STExcept where

import Control.Monad.Catch (Exception (toException), MonadThrow (..), SomeException)
import Data.Text (Text)
import qualified Network.HTTP.Client as NC
import Network.HTTP.Req (HttpException (JsonHttpException, VanillaHttpException))

data YnabAPIException
  = ServiceAPIError String
  | NetworkError SomeException
  | UnknownLocation Text
  | InvalidURL Text
  | InvalidPayloadData Text
  deriving (Exception)

instance Show YnabAPIException where
  show (ServiceAPIError _) = "Error communicating with external services"
  show (NetworkError _) = "Network communication error"
  show (UnknownLocation _) = "Failed to fetch budget"
  show (InvalidURL _) = "Found invalid URL in settings"
  show (InvalidPayloadData _) = "Payload returned is invalid"

rethrowReqException :: MonadThrow m => HttpException -> m a
rethrowReqException (JsonHttpException s) = throwM (ServiceAPIError s)
rethrowReqException
  ( VanillaHttpException
      ( NC.HttpExceptionRequest
          _
          (NC.StatusCodeException resp _)
        )
    ) =
    throwM (ServiceAPIError $ show $ NC.responseStatus resp)
rethrowReqException (VanillaHttpException e) = throwM (NetworkError $ toException e)
