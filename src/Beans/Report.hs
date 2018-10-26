module Beans.Report
  ( journal
  , balance
  , incomeStatement
  , balanceSheet
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
                                                          ( createJournal )
import qualified Beans.Report.Balance          as Balance
                                                          ( createBalance
                                                          , incomeStatement
                                                          , balanceSheet
                                                          )
import           Beans.Table                              ( showTable
                                                          , Table(..)
                                                          )
import qualified Data.Text.IO                  as TIO
import           Prelude                           hiding ( filter )


journal :: (MonadIO m, MonadThrow m) => JournalOptions -> m ()
journal options@JournalOptions {..} =
  L.fromFile jrnOptJournal
    >>= valuateLedger jrnOptMarket
    >>= Journal.createJournal options
    >>= printTable

balance :: (MonadIO m, MonadThrow m) => BalanceOptions -> m ()
balance options@BalanceOptions {..} =
  L.fromFile balOptJournal
    >>= valuateLedger balOptMarket
    >>= Balance.createBalance options
    >>= printTable

incomeStatement :: (MonadIO m, MonadThrow m) => BalanceOptions -> m ()
incomeStatement options@BalanceOptions {..} =
  L.fromFile balOptJournal
    >>= valuateLedger balOptMarket
    >>= Balance.incomeStatement options
    >>= printTable

balanceSheet :: (MonadIO m, MonadThrow m) => BalanceOptions -> m ()
balanceSheet options@BalanceOptions {..} =
  L.fromFile balOptJournal
    >>= valuateLedger balOptMarket
    >>= Balance.balanceSheet options
    >>= printTable

printTable :: (MonadIO m, Table a) => a -> m ()
printTable = liftIO . TIO.putStrLn . showTable
