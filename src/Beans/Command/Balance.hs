module Beans.Command.Balance
  ( run,
  )
where

import qualified Beans.Assertion as Assertion
import qualified Beans.Balance as Balance
import Beans.Balance (Balance (Balance))
import Beans.Command (Command (..), Directive (..))
import qualified Beans.Date as Date
import Beans.Filter (Filter)
import Beans.Ledger (Ledger)
import qualified Beans.Ledger as Ledger
import Beans.Options (BalanceOptions (..), Diffing (..))
import qualified Beans.Parser as Parser
import qualified Beans.Process as Process
import qualified Beans.Report as Report
import qualified Beans.Table as Table
import Beans.Table (Table)
import qualified Beans.Transaction as Transaction
import Beans.ValAmount (ValAmount)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text.IO as TextIO
import Prelude hiding (filter, sum)

run :: (MonadIO m, MonadThrow m, MonadReader BalanceOptions m) => m ()
run = do
  BalanceOptions {filter, journal} <- ask
  directives <- List.filter (matchDirective filter) <$> Parser.parseFile journal
  let ledger = Ledger.fromDirectives directives
  balances <- ledgerToBalance ledger
  report <- Report.fromBalances balances
  table <- Report.toTable report
  printTable table

matchDirective :: Filter -> Directive -> Bool
matchDirective flt (CmdDirective _ (CmdTransaction t)) = Transaction.match flt t
matchDirective flt (CmdDirective _ (CmdAssertion a)) = Assertion.match flt a
matchDirective _ _ = True

printTable :: (MonadIO m) => Table -> m ()
printTable = liftIO . TextIO.putStrLn . Table.display

ledgerToBalance :: (MonadReader BalanceOptions m, MonadThrow m) => Ledger -> m [Balance ValAmount]
ledgerToBalance l = do
  BalanceOptions {fromDate, diffing, period, percent, toDate, valuation} <- ask
  minDate <- Ledger.minDate l
  maxDate <- Ledger.maxDate l
  let t0 = Maybe.fromMaybe minDate fromDate
      t1 = Maybe.fromMaybe maxDate toDate
      interval = case period of
        Just i -> Date.partition i t0 t1
        Nothing -> [t0, t1]
      tminus = Date.addDays (-1) $ head interval
      ledgers = Ledger.splitAtDates l interval
      b = Balance.new tminus valuation
  (_, bs) <- unzip <$> Process.runLedgers b ledgers
  let bs' = zipWith (\(Balance _ a p pr np) d' -> Balance d' a p pr np) bs interval
      bs'' = case diffing of
        Diffing -> zipWith Balance.diff (tail bs') bs'
        NoDiffing -> tail bs'
  let bs''' = case percent of
        Nothing -> bs''
        Just a -> Balance.inPercent a <$> bs''
  pure $ Balance.eraseLots <$> bs'''
