module Beans.Report
  ( journal
  , balance
  )
where

import           Beans.Options                            ( JournalOptions(..)
                                                          , BalanceOptions(..)
                                                          )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import qualified Beans.Ledger                  as L
import           Beans.Accounts                           ( checkLedger )
import           Beans.Valuation                          ( valuateLedger )
import qualified Beans.Report.Journal          as Journal
                                                          ( createReport
                                                          , reportToTable
                                                          )
import qualified Beans.Report.Balance          as Balance
                                                          ( createReport
                                                          , reportToTable
                                                          )

import           Beans.Table                              ( showTable
                                                          , Cell
                                                          )
import qualified Data.Text.IO                  as TIO
import           Prelude                           hiding ( filter )


journal :: (MonadIO m, MonadThrow m) => JournalOptions -> m ()
journal options@JournalOptions {..} =
  L.fromFile jrnOptJournal
    >>= checkLedger
    >>= valuateLedger jrnOptMarket
    >>= Journal.createReport options
    >>= printTable
    .   Journal.reportToTable

balance :: (MonadIO m, MonadThrow m) => BalanceOptions -> m ()
balance options@BalanceOptions {..} =
  L.fromFile balOptJournal
    >>= checkLedger
    >>= valuateLedger balOptMarket
    >>= Balance.createReport options
    >>= printTable
    .   Balance.reportToTable


printTable :: (MonadIO m, MonadThrow m) => [[Cell]] -> m ()
printTable = liftIO . TIO.putStrLn . showTable
