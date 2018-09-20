module Beans.Valuation where

import           Beans.Data.Accounts     (AccountName (..), AccountType (..),
                                          Accounts, AccountsHistory, Amount,
                                          CommodityName (..), Lot (..))
import           Beans.Data.Directives   (Command (..), Flag (..), Open (..),
                                          Posting, Transaction (..),
                                          mkBalancedTransaction)
import qualified Beans.Data.Map          as M
import           Beans.Data.Restrictions (Restriction (..))
import           Beans.Ledger            (Ledger (..), Timestep (..))
import qualified Beans.Ledger            as L
import           Beans.Prices            (NormalizedPrices, Prices, lookupPrice,
                                          normalize, updatePrices)
import           Control.Monad.Catch     (MonadThrow)
import           Control.Monad.State     (MonadState, evalStateT, get, gets,
                                          put)
import           Data.Maybe              (catMaybes)
import           Data.Monoid             (Sum (Sum))
import           Data.Time.Calendar      (Day, fromGregorian)

data ValuationState = ValuationState
  { vsPrices               :: Prices
  , vsPrevNormalizedPrices :: NormalizedPrices
  , vsNormalizedPrices     :: NormalizedPrices
  , vsPrevAccounts         :: Accounts
  , vsAccounts             :: AccountsHistory
  , vsTarget               :: CommodityName
  , vsValuationAccount     :: AccountName
  , vsDate                 :: Day
  }

calculateValuation ::
     MonadThrow m
  => AccountsHistory
  -> CommodityName
  -> AccountName
  -> Ledger
  -> m Ledger
calculateValuation accounts target valuationAccount ledger =
  evalStateT
    (Ledger <$> mapM convertTimestep (L.toList ledger))
    ValuationState
      { vsPrices = mempty
      , vsPrevNormalizedPrices = mempty
      , vsNormalizedPrices = mempty
      , vsPrevAccounts = mempty
      , vsAccounts = accounts
      , vsDate = fromGregorian 1900 1 1
      , vsTarget = target
      , vsValuationAccount = valuationAccount
      }

convertTimestep ::
     (MonadThrow m, MonadState ValuationState m) => Timestep -> m Timestep
convertTimestep timestep@(Timestep day commands) = do
  ValuationState {..} <- get
  let accounts = M.lookupLEM day vsAccounts
      vsPrices' = updatePrices timestep vsPrices
  put
    ValuationState
      { vsPrices = vsPrices'
      , vsPrevNormalizedPrices = vsNormalizedPrices
      , vsNormalizedPrices = normalize vsPrices' vsTarget
      , vsPrevAccounts = accounts
      , ..
      }
  valuationTransactions <- adjustValuationForAccounts
  commands' <- concat <$> mapM process commands
  return $ Timestep day (commands' ++ valuationTransactions)

process :: (MonadThrow m, MonadState ValuationState m) => Command -> m [Command]
process (OpenCommand o) = do
  t <- gets vsTarget
  return [OpenCommand $ o {oRestriction = RestrictedTo [t]}]
process (TransactionCommand Transaction {..}) = do
  ValuationState {vsValuationAccount} <- get
  postings <- M.fromListM <$> mapM convertPosting (M.toList tPostings)
  t <-
    mkBalancedTransaction
      tFlag
      tDescription
      tTags
      postings
      (Just vsValuationAccount)
  return [TransactionCommand t]
process (BalanceCommand _) = pure []
process c = pure [c]

convertPosting ::
     (MonadThrow m, MonadState ValuationState m) => Posting -> m Posting
convertPosting p@((account, commodity, _), amount) = do
  tc <- gets vsTarget
  if tc == commodity
    then return p
    else do
      price <- gets vsNormalizedPrices >>= lookupPrice commodity
      return ((account, tc, Nothing), amount * Sum price)

adjustValuationForAccounts ::
     (MonadThrow m, MonadState ValuationState m) => m [Command]
adjustValuationForAccounts = do
  ValuationState {vsPrevAccounts} <- get
  s <- sequence $ adjustValuationForAccount <$> M.toList vsPrevAccounts
  return $ catMaybes s

adjustValuationForAccount ::
     (MonadThrow m, MonadState ValuationState m)
  => ((AccountName, CommodityName, Maybe Lot), Amount)
  -> m (Maybe Command)
adjustValuationForAccount ((a@(AccountName t _), c, _), s) = do
  v0 <- gets vsPrevNormalizedPrices >>= lookupPrice c
  v1 <- gets vsNormalizedPrices >>= lookupPrice c
  if v0 /= v1 && t `elem` [Assets, Liabilities]
    then Just <$> createValuationTransaction a (s * Sum (v1 - v0))
    else return Nothing

createValuationTransaction ::
     (MonadState ValuationState m, MonadThrow m)
  => AccountName
  -> Amount
  -> m Command
createValuationTransaction account amount = do
  ValuationState {vsTarget, vsValuationAccount} <- get
  let postings =
        M.fromListM
          [ ((account, vsTarget, Nothing), amount)
          , ((vsValuationAccount, vsTarget, Nothing), -amount)
          ]
  TransactionCommand <$>
    mkBalancedTransaction Complete "valuation" [] postings Nothing
