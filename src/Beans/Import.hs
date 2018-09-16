module Beans.Import
  ( importCommand
  ) where

import           Beans.Import.CH.Postfinance
import           Beans.Import.Common         (TransactionData (..))
import           Beans.Import.DSL            (evalRules, parseFile)
import           Beans.Options               (ImportOptions (..), Importer (..))
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, asks, runReaderT)

importCommand :: (MonadReader ImportOptions m, MonadThrow m, MonadIO m) => m ()
importCommand = do
  parse <- getParser
  entries <- tdEntries <$> ( asks optData >>= parse)
  evaluator <- evalRules <$> asks optAccount <*> (asks optConfig >>= parseFile)
  let accounts = runReaderT evaluator <$> entries
  liftIO $ print accounts

getParser
  :: (MonadThrow m, MonadIO m, MonadReader ImportOptions m)
  => m (FilePath -> m TransactionData)
getParser = select <$> asks optImporter
  where select Postfinance = Beans.Import.CH.Postfinance.readCSV
