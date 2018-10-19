module Beans.Report
  ( journal
  , balance
  , incomeStatement
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
import           Beans.Valuation                          ( valuateLedger )
import qualified Beans.Report.Journal          as Journal
                                                          ( createReport
                                                          , reportToTable
                                                          )
import qualified Beans.Report.Balance          as Balance
                                                          ( createReport
                                                          , incomeStatement
                                                          , incomeStatementToTable
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
    >>= valuateLedger jrnOptMarket
    >>= Journal.createReport options
    >>= printTable
    .   Journal.reportToTable

balance :: (MonadIO m, MonadThrow m) => BalanceOptions -> m ()
balance options@BalanceOptions {..} =
  L.fromFile balOptJournal
    >>= valuateLedger balOptMarket
    >>= Balance.createReport options
    >>= printTable
    .   Balance.reportToTable

incomeStatement :: (MonadIO m, MonadThrow m) => BalanceOptions -> m ()
incomeStatement options@BalanceOptions {..} =
  L.fromFile balOptJournal
    >>= valuateLedger balOptMarket
    >>= Balance.incomeStatement options
    >>= printTable
    .   Balance.incomeStatementToTable


printTable :: MonadIO m => [[Cell]] -> m ()
printTable = liftIO . TIO.putStrLn . showTable
