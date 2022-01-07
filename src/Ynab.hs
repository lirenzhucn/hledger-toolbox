{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Ynab where

import qualified Ynab.Types as YT
import Control.Monad.Reader (ReaderT (runReaderT), MonadIO, MonadReader)
import Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow )

newtype YnabApp a = YnabApp
    { runApp :: ReaderT YT.AppEnv IO a }
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadThrow, MonadCatch, MonadMask,
              MonadReader YT.AppEnv)

runYnabApp :: YnabApp a -> YT.AppEnv -> IO a
runYnabApp app = runReaderT (runApp app)
