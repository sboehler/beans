module Beans.Lib
  ( runBeans
  , Options(..)
  , Command(..)
  ) where

import           Beans.Accounts           (Accounts, AccountsHistory,
                                           calculateAccounts, diffAccounts)
import           Beans.AST                (AccountName (..), CommodityName (..))
import           Beans.Ledger             (Ledger, buildLedger)
import           Beans.Parser             (parseFile)
import           Beans.Report.Balance     (eraseLots, printAccounts, summarize)
import           Beans.Valuation          (calculateValuation)
import           Control.Monad.Catch      (MonadThrow)
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Reader     (MonadReader, asks, runReaderT)
import           Control.Monad.Trans      (liftIO)
import           Data.Bool                (bool)
import qualified Data.Map.Strict.Extended as M
import           Data.Time.Calendar       (Day)
import           Data.Time.LocalTime      (getZonedTime, localDay,
                                           zonedTimeToLocalTime)

data Options = Options
  { optJournal :: FilePath
  , optMarket  :: Maybe CommodityName
  , optLots    :: Bool
  , optFrom    :: Maybe Day
  , optTo      :: Maybe Day
  , optDepth   :: Maybe Int
  , optCommand :: Command
  } deriving (Show)

data Command =
  Balance
  deriving (Show)


runBeans :: (MonadIO m, MonadThrow m) => Options -> m ()
runBeans = runReaderT run

run :: (MonadIO m, MonadThrow m, MonadReader Options m) =>  m ()
run =
  parseStage >>= valuationStage >>= accountsStage >>= reportStage >>=
    aggregationStage >>=
    liftIO . printAccounts

parseStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => m Ledger
parseStage = buildLedger <$> (asks optJournal >>= parseFile)

valuationStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => Ledger -> m Ledger
valuationStage ledger = do
    let account = AccountName ["Equity", "Valuation"]
    asks optMarket >>= \m -> maybe pure (`calculateValuation` account) m ledger

accountsStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => Ledger -> m AccountsHistory
accountsStage = calculateAccounts

reportStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => AccountsHistory -> m Accounts
reportStage h = asks optCommand >>= \case
  Balance -> balanceReport h

balanceReport :: (MonadIO m, MonadThrow m, MonadReader Options m) => AccountsHistory -> m Accounts
balanceReport  accountsHistory = do
  a1 <- asks optTo >>= liftIO . maybe getDate pure >>= \to -> pure $ M.lookupLessEqual to accountsHistory
  a0 <- asks optFrom >>= \from -> pure $ maybe mempty (`M.lookupLessEqual` accountsHistory) from
  return $ a1 `diffAccounts` a0

aggregationStage :: (MonadIO m, MonadThrow m, MonadReader Options m) => Accounts -> m Accounts
aggregationStage a = do
  f1 <- bool eraseLots id <$> asks optLots
  s <- maybe id summarize <$> asks optDepth
  return $ (f1 . s) a

getDate :: IO Day
getDate = localDay . zonedTimeToLocalTime <$> getZonedTime
