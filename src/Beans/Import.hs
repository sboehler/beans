module Beans.Import
  ( importCommand
  ) where

import           Beans.DSL.Interpreter       (evalRules)
import           Beans.DSL.Parser            (parseFile)
import           Beans.Import.CH.Postfinance
import           Beans.Import.Common         (TransactionData (..))
import           Beans.Options               (ImportOptions (..), Importer (..))
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, ask, asks,
                                              runReaderT)

importCommand :: (MonadReader ImportOptions m, MonadThrow m, MonadIO m) => m ()
importCommand
  -- TODO: implement & remove test implementation
 = do
  ImportOptions { optConfig, optAccount, optData } <- ask
  parse <- getParser
  TransactionData { _currency, _entries } <- parse optData
  rules <- parseFile optConfig
  let e = evalRules optAccount rules
  let accounts = runReaderT e <$> _entries
  liftIO $ print accounts


getParser :: (MonadThrow m, MonadIO m, MonadReader ImportOptions m) => m (FilePath -> m TransactionData)
getParser = select <$> asks optImporter
  where
    select Postfinance = Beans.Import.CH.Postfinance.readCSV
