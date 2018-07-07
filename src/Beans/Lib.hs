module Beans.Lib
  ( runBeans
  , Options(..)
  , Command(..)
  ) where

import           Beans.Accounts         (calculateAccounts)
import           Beans.Balance          (balanceReport)
import           Beans.Data.Accounts    (AccountName (..), AccountType (..),
                                         Accounts, AccountsHistory, eraseLots,
                                         summarize)
import           Beans.Format           (createFlatReport,
                                         createHierarchicalReport, formatTable,
                                         reportToRows)
import           Beans.Ledger           (Ledger, buildLedger, filterLedger)
import           Beans.Options          (Command (..), Options (..),
                                         ReportType (..))
import           Beans.Parser           (parseFile)
import           Beans.Valuation        (calculateValuation)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, asks, runReaderT)
import           Control.Monad.Trans    (liftIO)
import qualified Data.Text.IO           as TIO
import           Prelude                hiding (filter)

runBeans :: (MonadIO m, MonadThrow m) => Options -> m ()
runBeans = runReaderT run

run :: (MonadIO m, MonadThrow m, MonadReader Options m) =>  m ()
run = do
  ledger <- parseStage
  accountsHistory <- accountsStage True ledger
  valuationStage accountsHistory ledger >>= filterStage >>= accountsStage False >>=
    reportStage >>=
    aggregationStage >>=
    printStage

parseStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => m Ledger
parseStage = do
  journal <- asks optJournal
  buildLedger <$> parseFile journal

filterStage :: (MonadReader Options m) => Ledger -> m Ledger
filterStage l = do
  filter <- asks optFilter
  strict <- asks optStrictFilter
  return $
    case filter of
      Just regex -> filterLedger strict regex l
      Nothing    -> l

valuationStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => AccountsHistory -> Ledger -> m Ledger
valuationStage accountsHistory ledger = do
  target <- asks optMarket
  case target of
    Just commodity ->
      calculateValuation
        accountsHistory
        commodity
        (AccountName Equity ["Valuation"])
        ledger
    Nothing -> pure ledger

accountsStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => Bool -> Ledger -> m AccountsHistory
accountsStage = calculateAccounts

reportStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => AccountsHistory -> m Accounts
reportStage h = do
  command <- asks optCommand
  case command of
    Balance -> balanceReport h

aggregationStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => Accounts -> m Accounts
aggregationStage accounts = do
  showLots <- asks optLots
  depth <- asks optDepth
  let eraseStage =
        if showLots
          then id
          else eraseLots
  let summarize' =
        case depth of
          Just d  -> summarize d
          Nothing -> id
  return $ (eraseStage . summarize') accounts

printStage :: (MonadReader Options m, MonadIO m) => Accounts -> m ()
printStage accounts = do
  reportType <- asks optReportType
  let createReport =
        case reportType of
          Hierarchical -> createHierarchicalReport
          Flat         -> createFlatReport
  (liftIO . TIO.putStrLn . formatTable . reportToRows . createReport) accounts
