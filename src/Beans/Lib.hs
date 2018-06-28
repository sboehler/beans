module Beans.Lib
  ( runBeans
  , Options(..)
  , Command(..)
  ) where

import           Beans.Accounts         (Accounts, AccountsHistory,
                                         calculateAccounts, eraseLots,
                                         summarize)
import           Beans.AST              (AccountName (..), AccountType (..))
import           Beans.Balance          (balanceReport)
import           Beans.Format           (createReport, formatTable,
                                         reportToRows)
import           Beans.Ledger           (Ledger, buildLedger)
import           Beans.Options          (Command (..), Options (..))
import           Beans.Parser           (parseFile)
import           Beans.Valuation        (calculateValuation)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, asks, runReaderT)
import           Control.Monad.Trans    (liftIO)
import           Data.Bool              (bool)

runBeans :: (MonadIO m, MonadThrow m) => Options -> m ()
runBeans = runReaderT run

run :: (MonadIO m, MonadThrow m, MonadReader Options m) =>  m ()
run =
  parseStage >>= valuationStage >>= accountsStage >>= reportStage >>=
  aggregationStage >>=
  liftIO . putStrLn . formatTable . reportToRows . createReport

parseStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => m Ledger
parseStage = buildLedger <$> (asks optJournal >>= parseFile)

valuationStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => Ledger -> m Ledger
valuationStage ledger = valuate ledger =<< asks optMarket
  where
    valuate l (Just m) = calculateValuation m (AccountName Equity ["Valuation"]) l
    valuate l Nothing = pure l

accountsStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => Ledger -> m AccountsHistory
accountsStage = calculateAccounts

reportStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => AccountsHistory -> m Accounts
reportStage h = asks optCommand >>= \case
  Balance -> balanceReport h

aggregationStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => Accounts -> m Accounts
aggregationStage accounts = do
  eraseStage <- bool eraseLots id <$> asks optLots
  summarizeStage <- maybe id summarize <$> asks optDepth
  return $ (eraseStage . summarizeStage) accounts

