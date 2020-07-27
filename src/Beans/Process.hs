module Beans.Process
  ( runLedger,
    runLedgers,
    ProcessException,
  )
where

import Beans.Account (Account, AccountType (..))
import Beans.Amount (Amount)
import Beans.Assertion (Assertion (..))
import Beans.Balance (Balance (Balance))
import qualified Beans.Balance as Balance
import Beans.Close (Close (..))
import Beans.Ledger (Ledger)
import qualified Beans.Ledger as Ledger
import Beans.LedgerStep (LedgerStep (LS))
import qualified Beans.LedgerStep as LedgerStep
import Beans.Open (Open (..))
import Beans.Position (Position (..))
import qualified Beans.Positions as Positions
import Beans.Price (Price)
import Beans.Prices (NormalizedPrices (NormalizedPrices))
import qualified Beans.Prices as Prices
import Beans.Transaction (Posting (..), Transaction (..))
import qualified Beans.Transaction as Transaction
import Beans.ValAmount (ValAmount (ValAmount))
import qualified Beans.ValAmount as ValAmount
import Control.Monad (foldM, unless, when)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State
import Data.Foldable (for_)
import Data.Group (invert)
import qualified Data.Map.Strict.Extended as Map
import qualified Data.Set as Set
import Data.Traversable (for)

data ProcessException
  = AccountIsNotOpen Close
  | BookingErrorAccountNotOpen Transaction Account
  | BookingErrorCommodityIncompatible Position
  | AccountIsAlreadyOpen Open
  | BalanceIsNotZero Close
  | AccountDoesNotExist Assertion
  | AssertionFailed Assertion Amount
  deriving (Show)

instance Exception ProcessException

runLedger :: (MonadThrow m) => Balance ValAmount -> Ledger -> m (Ledger, Balance ValAmount)
runLedger balance ledger = State.runStateT (Ledger.transform process ledger) balance

runLedgers :: (MonadThrow m) => Balance ValAmount -> [Ledger] -> m [(Ledger, Balance ValAmount)]
runLedgers balance ledgers = State.evalStateT (for ledgers go) balance
  where
    go ledger = (,) <$> Ledger.transform process ledger <*> State.get

--------------------------------------------------------------------------------
process :: (MonadThrow m, MonadState (Balance ValAmount) m) => LedgerStep -> m LedgerStep
process s = do
  let pipeline =
        -- PROCESSING PIPELINE
        sequence_
          [ -- update the date
            processDate,
            -- update the prices
            processPrices,
            -- update normalized prices
            normalizePrices,
            -- valuate ledgerstep (updating transactions)
            valuateTransactions,
            -- process openings
            processOpenings,
            -- process transactions to compute balance'
            processTransactions,
            -- process balance assertions
            processAssertions,
            -- valuate {asset, liability} positions, compute valuation adjustments
            createValAdjustments,
            -- process closings
            processClosings
          ]
  b <- State.get
  State balance step <- State.execStateT pipeline (State b s)
  State.put balance
  pure step

--------------------------------------------------------------------------------
data State
  = State (Balance ValAmount) LedgerStep
  deriving (Show)

--------------------------------------------------------------------------------

processDate :: MonadState State m => m ()
processDate =
  State.modify (\(State (Balance _ p a pr np) s@(LS d' _ _ _ _ _)) -> State (Balance d' p a pr np) s)

processPrices :: (MonadThrow m, MonadState State m) => m ()
processPrices = do
  State balance (LS _ _ p _ _ _) <- State.get
  balance' <- foldM processPrice balance p
  State.modify $ \(State _ step) -> State balance' step

normalizePrices :: (MonadThrow m, MonadState State m) => m ()
normalizePrices = do
  State (Balance d p a pr np) _ <- State.get
  let balance' = Balance d p a pr (Prices.normalize pr . getCommodity <$> np)
  State.modify $ \(State _ s) -> State balance' s
  where
    getCommodity (NormalizedPrices c _) = c

valuateTransactions :: (MonadThrow m, MonadState State m) => m ()
valuateTransactions = do
  State (Balance _ _ _ _ val) step <- State.get
  step' <- foldM LedgerStep.valuate step val
  State.modify $ \(State b _) -> State b step'

processOpenings :: (MonadThrow m, MonadState State m) => m ()
processOpenings = do
  State balance (LS _ o _ _ _ _) <- State.get
  balance' <- foldM processOpen balance o
  State.modify $ \(State _ s) -> State balance' s

processTransactions :: (MonadThrow m, MonadState State m) => m ()
processTransactions = do
  State balance (LS _ _ _ t _ _) <- State.get
  balance' <- foldM processTransaction balance t
  State.modify $ \(State _ s) -> State balance' s

processAssertions :: (MonadThrow m, MonadState State m) => m ()
processAssertions = do
  State balance (LS _ _ _ _ b _) <- State.get
  for_ b $ processAssertion balance

processClosings :: (MonadThrow m, MonadState State m) => m ()
processClosings = do
  State balance (LS _ _ _ _ _ c) <- State.get
  balance' <- foldM processClose balance c
  State.modify $ \(State _ s) -> State balance' s

createValAdjustments :: (MonadState State m, MonadThrow m) => m ()
createValAdjustments = do
  State bal@(Balance date positions _ _ valuations) step <- State.get
  let pos = Positions.filterByAccountType [Assets, Liabilities] positions
  pos' <- foldM Positions.valuate pos valuations
  let diffs = Map.filter (any (/= 0) . (\(ValAmount _ v) -> v)) (Map.unionWith (<>) pos' (invert <$> pos))
  transactions <- for (Map.toList diffs) $ Transaction.createAdjustment date
  balance' <- foldM processTransaction bal transactions
  let step' = LedgerStep.add transactions step
  State.modify $ const $ State balance' step'

--------------------------------------------------------------------------------

processClose :: (MonadThrow m) => Balance ValAmount -> Close -> m (Balance ValAmount)
processClose (Balance date positions accounts prices valuations) close@(Close _ a) = do
  unless (a `Set.member` accounts) $ throwM $ AccountIsNotOpen close
  let (selected, others) = Positions.partitionByAccount a positions
  unless (all ValAmount.amountIsZero selected) $ throwM $ BalanceIsNotZero close
  pure $ Balance date others (Set.delete a accounts) prices valuations

processTransaction :: (MonadThrow m) => Balance ValAmount -> Transaction -> m (Balance ValAmount)
processTransaction bal@(Balance _ _ acc _ _) t@(Transaction _ _ _ p) = do
  for_ p canBook
  pure $ Balance.book bal p
  where
    canBook (Posting a _ _ _ _) =
      unless (a `Set.member` acc) $
        throwM $
          BookingErrorAccountNotOpen t a

processAssertion :: MonadThrow m => Balance ValAmount -> Assertion -> m ()
processAssertion (Balance _ positions _ _ _) assertion@(Assertion _ a amt c) = do
  let s = (\(ValAmount a' _) -> a') $ Positions.sum a c positions
  unless (s == amt) $ throwM $ AssertionFailed assertion s

processOpen :: (MonadThrow m) => Balance a -> Open -> m (Balance a)
processOpen (Balance d positions acc pr np) open@(Open _ a) = do
  when (a `Set.member` acc) $ throwM $ AccountIsAlreadyOpen open
  pure $ Balance d positions (Set.insert a acc) pr np

processPrice :: MonadThrow m => Balance a -> Price -> m (Balance a)
processPrice (Balance d p a pr np) price = do
  pr' <- Prices.updatePrices pr price
  pure $ Balance d p a pr' np
