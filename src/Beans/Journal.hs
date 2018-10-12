module Beans.Journal
  ( journalCommand
  )
where

import           Beans.Data.Directives                    ( Dated(..) )
import           Beans.Options                            ( JournalOptions(..)
                                                          , Valuation(..)
                                                          )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Control.Monad.Reader                     ( MonadReader
                                                          , asks
                                                          )
import           Beans.Parser                             ( parseFile )
import           Beans.Ledger                             ( Ledger
                                                          , buildLedger
                                                          , filterLedger
                                                          )
import           Beans.Accounts                           ( checkLedger )
import           Beans.Valuation                          ( valuateLedger )
import qualified Data.List                     as L
import           Beans.Pretty                             ( prettyPrintLedger )
import           Beans.Data.Accounts                      ( Account(..)
                                                          , AccountType(..)
                                                          )

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
parseStage = buildLedger <$> (asks jrnOptJournal >>= parseFile)

valuationStage
  :: (MonadThrow m, MonadReader JournalOptions m) => Ledger -> m Ledger
valuationStage ledger = asks jrnOptMarket >>= f
 where
  f (AtMarket commodity) =
    valuateLedger commodity (Account Equity ["Valuation"]) ledger
  f _ = pure ledger

filterStage :: MonadReader JournalOptions f => Ledger -> f Ledger
filterStage l = flip filterLedger l <$> asks jrnOptFilter
