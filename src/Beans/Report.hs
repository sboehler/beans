module Beans.Report
  ( journal
  , balance
  , incomeStatement
  , balanceSheet
  )
where

import           Beans.Options                  ( JournalOptions(..)
                                                , BalanceOptions(..)
                                                )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Beans.Valuation                ( valuateLedger )
import qualified Beans.Report.Journal          as Journal
                                                ( createJournal )
import           Beans.Model                    ( build )
import           Beans.Parser                   ( parseFile )
import qualified Beans.Report.Balance          as Balance
                                                ( createBalance
                                                , incomeStatement
                                                , balanceSheet
                                                )
import           Beans.Table                    ( showTable
                                                , Table(..)
                                                )
import qualified Data.Text.IO                  as TIO


journal :: (MonadIO m, MonadThrow m) => JournalOptions -> m ()
journal options@JournalOptions {..} =
  build
    <$> parseFile jrnOptJournal
    >>= valuateLedger jrnOptMarket
    >>= Journal.createJournal options
    >>= printTable

balance :: (MonadIO m, MonadThrow m) => BalanceOptions -> m ()
balance options@BalanceOptions {..} =
  build
    <$> parseFile balOptJournal
    >>= valuateLedger balOptMarket
    >>= Balance.createBalance options
    >>= printTable

incomeStatement :: (MonadIO m, MonadThrow m) => BalanceOptions -> m ()
incomeStatement options@BalanceOptions {..} =
  build
    <$> parseFile balOptJournal
    >>= valuateLedger balOptMarket
    >>= Balance.incomeStatement options
    >>= printTable

balanceSheet :: (MonadIO m, MonadThrow m) => BalanceOptions -> m ()
balanceSheet options@BalanceOptions {..} =
  build
    <$> parseFile balOptJournal
    >>= valuateLedger balOptMarket
    >>= Balance.balanceSheet options
    >>= printTable

printTable :: (MonadIO m, Table a) => a -> m ()
printTable = liftIO . TIO.putStrLn . showTable
