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
import           Beans.Format           (createReport, formatTable,
                                         reportToRows)
import           Beans.Ledger           (Ledger, buildLedger, filterLedger)
import           Beans.Options          (Command (..), Options (..))
import           Beans.Parser           (parseFile)
import           Beans.Valuation        (calculateValuation)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, asks, runReaderT)
import           Control.Monad.Trans    (liftIO)
import           Data.Bool              (bool)
import qualified Data.Text.IO           as TIO

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
parseStage = buildLedger <$> (asks optJournal >>= parseFile)

filterStage :: (MonadReader Options m) => Ledger -> m Ledger
filterStage l = do
  f <- asks optFilter
  s <- asks optStrictFilter
  return $
    case f of
      Just f' -> filterLedger s f' l
      Nothing -> l

valuationStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => AccountsHistory -> Ledger -> m Ledger
valuationStage accountsHistory ledger = valuate =<< asks optMarket
  where
    valuate (Just m) = calculateValuation accountsHistory m (AccountName Equity ["Valuation"]) ledger
    valuate Nothing = pure ledger

accountsStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => Bool -> Ledger -> m AccountsHistory
accountsStage = calculateAccounts

reportStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => AccountsHistory -> m Accounts
reportStage h = asks optCommand >>= \case
  Balance -> balanceReport h

aggregationStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => Accounts -> m Accounts
aggregationStage accounts = do
  eraseStage <- bool eraseLots id <$> asks optLots
  summarizeStage <- maybe id summarize <$> asks optDepth
  return $ (eraseStage . summarizeStage) accounts

printStage :: (MonadIO m) => Accounts -> m ()
printStage = liftIO . TIO.putStrLn . formatTable . reportToRows . createReport
