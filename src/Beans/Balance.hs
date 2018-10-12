module Beans.Balance
  ( balanceCommand
  )
where

import           Beans.Accounts                           ( calculateAccountsForDays
                                                          , checkLedger
                                                          )
import           Beans.Data.Accounts                      ( Account(..)
                                                          , Accounts
                                                          , eraseLots
                                                          , summarize
                                                          )
import qualified Beans.Data.Map                as M
import           Beans.Format                             ( createReport
                                                          , formatTable
                                                          , reportToRows
                                                          )
import           Beans.Ledger                             ( Ledger
                                                          , buildLedger
                                                          , filterLedger
                                                          )
import           Beans.Options                            ( BalanceOptions(..)
                                                          , ReportType(..)
                                                          )
import           Beans.Parser                             ( parseFile )
import           Beans.Valuation                          ( valuateLedger )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.IO.Class                   ( MonadIO
                                                          , liftIO
                                                          )
import           Control.Monad.Reader                     ( MonadReader
                                                          , asks
                                                          )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO


balanceCommand
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m) => m ()
balanceCommand =
  parseStage
    >>= checkLedger
    >>= valuationStage
    >>= filterStage
    >>= reportStage
    >>= aggregationStage
    >>= printStage

parseStage
  :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m) => m Ledger
parseStage = buildLedger <$> (asks balOptJournal >>= parseFile)

filterStage :: (MonadReader BalanceOptions m) => Ledger -> m Ledger
filterStage l = flip filterLedger l <$> asks balOptFilter

valuationStage
  :: (MonadThrow m, MonadReader BalanceOptions m) => Ledger -> m Ledger
valuationStage ledger = asks balOptMarket >>= flip valuateLedger ledger

reportStage
  :: (MonadThrow m, MonadReader BalanceOptions m) => Ledger -> m Accounts
reportStage ledger = do
  to       <- asks balOptTo
  from     <- asks balOptFrom
  [a0, a1] <- calculateAccountsForDays ledger [from, to] mempty
  return $ M.filter (not . null) $ fmap (M.filter (/= 0)) $ a1 `M.minus` a0

aggregationStage :: (MonadReader BalanceOptions m) => Accounts -> m Accounts
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
