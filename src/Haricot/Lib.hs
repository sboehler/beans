module Haricot.Lib
  ( parse
  , Options(..)
  , Command(..)
  ) where

import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans    (liftIO)
import qualified Data.Map.Strict        as M
import           Data.Time.Calendar     (Day, fromGregorian)
import           Haricot.Accounts       (calculateAccounts)
import           Haricot.AST            (AccountName (..), CommodityName (..))
import           Haricot.Ledger         (buildLedger)
import           Haricot.Parser         (parseFile)
import           Haricot.Report.Balance (eraseLots, printAccounts, summarize)
import           Haricot.Valuation      (calculateValuation)

data Options = Options
  { optJournal :: FilePath
  , optMarket  :: Maybe CommodityName
  , optFrom    :: Maybe Day
  , optTo      :: Maybe Day
  , optCommand :: Command
  } deriving (Show)

data Command =
  Balance
  deriving (Show)

parse :: (MonadIO m, MonadThrow m) => Options -> m ()
parse Options{..}= do
  ledger <- buildLedger <$> parseFile optJournal
  let target = CommodityName "CHF"
      account = AccountName ["Equity", "Valuation"]
  valHistory <- calculateAccounts =<< calculateValuation target account ledger
  let accounts = M.lookupLE (fromGregorian 2017 12 9) valHistory
  case accounts of
    Just (_, a) -> liftIO $ printAccounts $ eraseLots <$> summarize 2 a
    Nothing     -> return ()
