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
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Time.Calendar     (Day)
import           Data.Time.LocalTime    (getZonedTime, localDay,
                                         zonedTimeToLocalTime)

balanceCommand
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m) => m ()
balanceCommand =
  parseStage
    >>= valuationStage
    >>= filterStage
    >>= accountsStage
    >>= reportStage
    >>= aggregationStage
    >>= printStage

parseStage
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m) => m Ledger
parseStage = buildLedger <$> (asks balOptJournal >>= parseFile)

filterStage :: (MonadReader BalanceOptions m) => Ledger -> m Ledger
filterStage l = f <$> asks balOptFilter
 where
  f (StrictFilter regex) = filterLedger True regex l
  f (Filter       regex) = filterLedger False regex l
  f NoFilter             = l

valuationStage
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m)
  => Ledger
  -> m Ledger
valuationStage ledger = asks balOptMarket >>= f
 where
  f (AtMarket commodity) =
    calculateValuation commodity (Account Equity ["Valuation"]) ledger
  f _ = pure ledger

accountsStage
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m)
  => Ledger
  -> m AccountsHistory
accountsStage = calculateAccounts

reportStage
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m)
  => AccountsHistory
  -> m Accounts
reportStage accountsHistory = do
  today <- liftIO getDate
  to    <- asks balOptTo
  from  <- asks balOptFrom
  let a1 = M.lookupLEM (fromMaybe today to) accountsHistory
      a0 = maybe mempty (`M.lookupLEM` accountsHistory) from
  return $ M.filter (not . null) $ fmap (M.filter (/= 0)) $ a1 `M.minus` a0

getDate :: IO Day
getDate = localDay . zonedTimeToLocalTime <$> getZonedTime

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
