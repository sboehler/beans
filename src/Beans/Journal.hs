module Beans.Journal
  ( journalCommand
  )
where

import           Beans.Data.Directives                    ( Dated(..)
                                                          , Command(Transaction)
                                                          )
import           Beans.Options                            ( JournalOptions(..) )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Control.Monad.Reader                     ( MonadReader
                                                          , ask
                                                          , asks
                                                          )
import           Beans.Parser                             ( parseFile )
import qualified Beans.Ledger                  as L
import           Beans.Ledger                             ( Ledger )
import           Beans.Accounts                           ( checkLedger )
import           Beans.Valuation                          ( valuateLedger )
import           Beans.Report.Journal                     ( createReport
                                                          , reportToTable
                                                          )
import           Beans.Table                              ( showTable )
import qualified Data.Text.IO                  as TIO


journalCommand
  :: (MonadIO m, MonadThrow m, MonadReader JournalOptions m) => m ()
journalCommand =
  parseStage >>= checkLedger >>= valuationStage >>= filterStage >>= reportStage

reportStage
  :: (MonadIO m, MonadThrow m, MonadReader JournalOptions m) => Ledger -> m ()
reportStage ledger = do
  options <- ask
  report  <- createReport options ledger
  (liftIO . TIO.putStrLn . showTable . reportToTable) report

parseStage
  :: (MonadIO m, MonadThrow m, MonadReader JournalOptions m) => m Ledger
parseStage = L.build <$> (asks jrnOptJournal >>= parseFile)

valuationStage
  :: (MonadThrow m, MonadReader JournalOptions m) => Ledger -> m Ledger
valuationStage ledger = asks jrnOptMarket >>= flip valuateLedger ledger

filterStage :: MonadReader JournalOptions m => Ledger -> m Ledger
filterStage l = do
  let transactions = [ x | x@(Dated _ Transaction{}) <- l ]
  filterConfig <- asks jrnOptFilter
  return $ L.filter filterConfig transactions
