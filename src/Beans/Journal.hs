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
                                                          , asks
                                                          )
import           Beans.Parser                             ( parseFile )
import qualified Beans.Ledger                  as BL
import           Beans.Ledger                             ( Ledger )
import           Beans.Accounts                           ( checkLedger )
import           Beans.Valuation                          ( valuateLedger )
import qualified Data.List                     as L
import           Beans.Pretty                             ( prettyPrintLedger )

journalCommand
  :: (MonadIO m, MonadThrow m, MonadReader JournalOptions m) => m ()
journalCommand =
  parseStage
    >>= checkLedger
    >>= valuationStage
    >>= filterStage
    >>= reportStage
    >>= liftIO
    .   prettyPrintLedger

reportStage
  :: (MonadThrow m, MonadReader JournalOptions m) => Ledger -> m Ledger
reportStage ledger = do
  to   <- asks jrnOptTo
  from <- asks jrnOptFrom
  return $ L.filter (\(Dated d _) -> d >= from && d <= to) ledger

parseStage
  :: (MonadIO m, MonadThrow m, MonadReader JournalOptions m) => m Ledger
parseStage = BL.build <$> (asks jrnOptJournal >>= parseFile)

valuationStage
  :: (MonadThrow m, MonadReader JournalOptions m) => Ledger -> m Ledger
valuationStage ledger = asks jrnOptMarket >>= flip valuateLedger ledger

filterStage :: MonadReader JournalOptions m => Ledger -> m Ledger
filterStage l = do
  let transactions = [ x | x@(Dated _ Transaction{}) <- l ]
  filterConfig <- asks jrnOptFilter
  return $ BL.filter filterConfig transactions
