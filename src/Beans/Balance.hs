module Beans.Balance
  ( balanceCommand
  ) where

import           Beans.Accounts         (calculateAccounts)
import           Beans.Data.Accounts    (Account (..), AccountType (..),
                                         Accounts, AccountsHistory, eraseLots,
                                         summarize)
import qualified Beans.Data.Map         as M
import           Beans.Format           (createReport, formatTable,
                                         reportToRows)
import           Beans.Ledger           (Ledger, buildLedger, filterLedger)
import           Beans.Options          (BalanceOptions (..), Filter (..),
                                         ReportType (..), Valuation (..))
import           Beans.Parser           (parseFile)
import           Beans.Valuation        (calculateValuation)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Time.Calendar     (Day)
import           Data.Time.LocalTime    (getZonedTime, localDay,
                                         zonedTimeToLocalTime)

reportStage
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m)
  => AccountsHistory
  -> m Accounts
reportStage accountsHistory = do
  to   <- maybe (liftIO getDate) pure =<< asks balOptTo
  from <- asks balOptFrom
  let a1 = M.lookupLEM to accountsHistory
      a0 = maybe mempty (`M.lookupLEM` accountsHistory) from
  return $ M.filter (not . null) $ fmap (M.filter (/= 0)) $ a1 `M.minus` a0

getDate :: IO Day
getDate = localDay . zonedTimeToLocalTime <$> getZonedTime

balanceCommand
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m) => m ()
balanceCommand = do
  ledger          <- parseStage
  accountsHistory <- accountsStage ledger
  valuationStage accountsHistory ledger
    >>= filterStage
    >>= accountsStage
    >>= reportStage
    >>= aggregationStage
    >>= printStage

parseStage
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m) => m Ledger
parseStage = do
  journal <- asks balOptJournal
  buildLedger <$> parseFile journal

filterStage :: (MonadReader BalanceOptions m) => Ledger -> m Ledger
filterStage l = do
  filter' <- asks balOptFilter
  return $ case filter' of
    StrictFilter regex -> filterLedger True regex l
    Filter       regex -> filterLedger False regex l
    NoFilter           -> l

valuationStage
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m)
  => AccountsHistory
  -> Ledger
  -> m Ledger
valuationStage accountsHistory ledger = do
  target <- asks balOptMarket
  case target of
    AtMarket commodity -> calculateValuation accountsHistory
                                             commodity
                                             (Account Equity ["Valuation"])
                                             ledger
    _ -> pure ledger

accountsStage
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m)
  => Ledger
  -> m AccountsHistory
accountsStage = calculateAccounts

aggregationStage
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m)
  => Accounts
  -> m Accounts
aggregationStage accounts = do
  showLots <- asks balOptLots
  depth    <- asks balOptDepth
  let eraseStage = if showLots then id else eraseLots
  let summarize' = case depth of
        Just d  -> summarize d
        Nothing -> id
  return $ (eraseStage . summarize') accounts

printStage :: (MonadReader BalanceOptions m, MonadIO m) => Accounts -> m ()
printStage accounts = do
  reportType <- asks balOptReportType
  let f = case reportType of
        Hierarchical -> hierarchical
        Flat         -> flat
  (liftIO . TIO.putStrLn . formatTable . reportToRows . createReport f) accounts
 where
  hierarchical (Account t ns, _, _) = T.pack (show t) : ns
  flat (a, _, _) = [T.pack $ show a]
