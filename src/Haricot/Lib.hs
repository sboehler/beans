module Haricot.Lib
  ( parse
  , Options(..)
  , Command(..)
  ) where

import           Control.Monad.Catch      (MonadThrow)
import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Trans      (liftIO)
import qualified Data.Map.Strict.Extended as M
import           Data.Time.Calendar       (Day)
import           Data.Time.LocalTime      (getZonedTime, localDay,
                                           zonedTimeToLocalTime)
import           Haricot.Accounts         (calculateAccounts, diffAccounts)
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
  , optDepth   :: Maybe Int
  , optCommand :: Command
  } deriving (Show)

data Command =
  Balance
  deriving (Show)

parse :: (MonadIO m, MonadThrow m) => Options -> m ()
parse Options {..} = do
  ledger <- buildLedger <$> parseFile optJournal
  ledger' <-
    let account = AccountName ["Equity", "Valuation"]
     in maybe pure (`calculateValuation` account) optMarket ledger
  history <- calculateAccounts ledger'
  to <- liftIO $ maybe getDate pure optTo
  let accounts1 = M.lookupLessEqual to history
      accounts0 = maybe mempty (`M.lookupLessEqual` history) optFrom
  liftIO $
    printAccounts $
    (if optLots
       then id
       else eraseLots) <$>
    maybe id summarize optDepth (accounts1 `diffAccounts` accounts0)

getDate :: IO Day
getDate = localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
