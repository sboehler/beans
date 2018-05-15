module Haricot.Lib
  ( parse
  , Options(..)
  , Command(..)
  ) where

import           Control.Monad.Catch      (MonadThrow)
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Trans      (liftIO)
import qualified Data.Map.Strict.Extended as M
import           Data.Maybe               (fromMaybe)
import           Data.Time.Calendar       (Day)
import           Data.Time.LocalTime      (getZonedTime, localDay,
                                           zonedTimeToLocalTime)
import           Haricot.Accounts         (calculateAccounts)
import           Haricot.AST              (AccountName (..), CommodityName (..))
import           Haricot.Ledger           (buildLedger)
import           Haricot.Parser           (parseFile)
import           Haricot.Report.Balance   (eraseLots, printAccounts, summarize)
import           Haricot.Valuation        (calculateValuation)

data Options = Options
  { optJournal :: FilePath
  , optMarket  :: Maybe CommodityName
  , optLots    :: Bool
  , optFrom    :: Maybe Day
  , optTo      :: Maybe Day
  , optCommand :: Command
  } deriving (Show)

data Command =
  Balance
  deriving (Show)

parse :: (MonadIO m, MonadThrow m) => Options -> m ()
parse Options {..} = do
  ledger <- buildLedger <$> parseFile optJournal
  ledger' <-
    case optMarket of
      Nothing -> pure ledger
      Just c ->
        let account = AccountName ["Equity", "Valuation"]
         in calculateValuation c account ledger
  history <- calculateAccounts ledger'
  to <- (`fromMaybe` optTo) <$> liftIO getDate
  let accounts = M.lookupLessEqual to history
  liftIO $
    printAccounts $
    (if optLots
       then id
       else eraseLots) <$>
    summarize 2 accounts

getDate :: IO Day
getDate = localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
