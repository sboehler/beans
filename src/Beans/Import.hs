module Beans.Import
  ( importCommand
  ) where

import           Beans.Import.CH.Postfinance
import           Beans.Import.Common         (TransactionData (..))
import           Beans.Import.DSL            (evaluate, parseFile)
import           Beans.Options               (ImportOptions (..), Importer (..))
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader        (MonadReader, asks)
import           Data.Maybe                  (fromMaybe)

importCommand :: (MonadReader ImportOptions m, MonadThrow m, MonadIO m) => m ()
importCommand = do
  parse <- getParser
  entries <- tdEntries <$> (asks optData >>= parse)
  rules <- asks optConfig >>= parseFile
  defaultAccount <- asks optAccount
  let accounts = fromMaybe defaultAccount . evaluate rules <$> entries
  liftIO $ print entries
  liftIO $ print accounts
  liftIO $ print rules

getParser
  :: (MonadThrow m, MonadIO m, MonadReader ImportOptions m)
  => m (FilePath -> m TransactionData)
getParser = select <$> asks optImporter
  where select Postfinance = Beans.Import.CH.Postfinance.readCSV
