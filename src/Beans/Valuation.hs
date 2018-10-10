module Beans.Valuation
  ( calculateValuation
  )
where

import           Beans.Data.Restrictions                  ( Restrictions )
import           Beans.Accounts                           ( checkTimestep
                                                          , processTimestep
                                                          )
import           Beans.Data.Accounts                      ( Account(..)
                                                          , AccountType(..)
                                                          , Accounts
                                                          , Amount
                                                          , Amounts
                                                          , Commodity(..)
                                                          , Lot(..)
                                                          )
import           Beans.Data.Directives                    ( Command(..)
                                                          , Flag(..)
                                                          , Posting
                                                          , Transaction(..)
                                                          , mkBalancedTransaction
                                                          )
import qualified Beans.Data.Map                as M
import           Beans.Ledger                             ( Ledger
                                                          , Timestep(..)
                                                          )
import           Beans.Prices                             ( NormalizedPrices
                                                          , Prices
                                                          , lookupPrice
                                                          , normalize
                                                          , updatePrices
                                                          )
import           Control.Monad.Catch                      ( MonadThrow )
import           Control.Monad.State                      ( MonadState
                                                          , evalStateT
                                                          , get
                                                          , gets
                                                          , put
                                                          )
import           Data.Monoid                              ( Sum(Sum) )
import           Data.Time.Calendar                       ( Day
                                                          , fromGregorian
                                                          )

data ValuationState = ValuationState
  { vsPrices               :: Prices
  , vsPrevNormalizedPrices :: NormalizedPrices
  , vsNormalizedPrices     :: NormalizedPrices
  , vsPrevAccounts         :: Accounts
  , vsTarget               :: Commodity
  , vsValuationAccount     :: Account
  , vsDate                 :: Day
  , vsRestrictions :: Restrictions
  }

calculateValuation :: MonadThrow m => Commodity -> Account -> Ledger -> m Ledger
calculateValuation target valuationAccount ledger = evalStateT
  (mapM convertTimestep ledger)
  ValuationState
    { vsPrices               = mempty
    , vsPrevNormalizedPrices = mempty
    , vsNormalizedPrices     = mempty
    , vsPrevAccounts         = mempty
    , vsDate                 = fromGregorian 1900 1 1
    , vsTarget               = target
    , vsValuationAccount     = valuationAccount
    , vsRestrictions         = mempty
    }

convertTimestep
  :: (MonadThrow m, MonadState ValuationState m) => Timestep -> m Timestep
convertTimestep timestep@(Timestep day commands) = do
  ValuationState {..} <- get
  let vsPrices' = updatePrices timestep vsPrices
  vsRestrictions' <- checkTimestep vsRestrictions timestep
  accounts        <- processTimestep vsPrevAccounts timestep
  put ValuationState
    { vsPrices               = vsPrices'
    , vsPrevNormalizedPrices = vsNormalizedPrices
    , vsNormalizedPrices     = normalize vsPrices' vsTarget
    , vsPrevAccounts         = accounts
    , vsRestrictions         = vsRestrictions'
    , ..
    }
  valuationTransactions <- adjustValuationForAccounts
  commands'             <- concat <$> mapM process commands
  return $ Timestep day (commands' ++ valuationTransactions)

process :: (MonadThrow m, MonadState ValuationState m) => Command -> m [Command]
process (TransactionCommand Transaction {..}) = do
  ValuationState { vsValuationAccount } <- get
  postings <- M.fromListM <$> mapM convertPosting (M.toList tPostings)
  t        <- mkBalancedTransaction tFlag
                                    tDescription
                                    tTags
                                    postings
                                    (Just vsValuationAccount)
  return [TransactionCommand t]
process (BalanceCommand _) = pure []
process c                  = pure [c]

convertPosting
  :: (MonadThrow m, MonadState ValuationState m) => Posting -> m Posting
convertPosting (k, amounts) = do
  e <- mapM convertAmount $ M.toList amounts
  return (k, M.fromListM e)

convertAmount
  :: (MonadThrow m, MonadState ValuationState m)
  => (Commodity, Amount)
  -> m (Commodity, Amount)
convertAmount a@(commodity, amount) = do
  tc <- gets vsTarget
  if tc == commodity
    then return a
    else do
      price <- gets vsNormalizedPrices >>= lookupPrice commodity
      return (tc, amount * Sum price)

adjustValuationForAccounts
  :: (MonadThrow m, MonadState ValuationState m) => m [Command]
adjustValuationForAccounts = do
  a <- M.toList <$> gets vsPrevAccounts
  concat <$> sequence (adjustValuationForAccount <$> a)

adjustValuationForAccount
  :: (MonadThrow m, MonadState ValuationState m)
  => ((Account, Commodity, Maybe Lot), Amounts)
  -> m [Command]
adjustValuationForAccount (k, amounts) =
  concat <$> mapM (adjustValuationForAmount k) (M.toList amounts)

adjustValuationForAmount
  :: (MonadThrow m, MonadState ValuationState m)
  => (Account, Commodity, Maybe Lot)
  -> (Commodity, Amount)
  -> m [Command]
adjustValuationForAmount k@(Account t _, _, _) (commodity, amount) = do
  v0 <- gets vsPrevNormalizedPrices >>= lookupPrice commodity
  v1 <- gets vsNormalizedPrices >>= lookupPrice commodity
  if v0 /= v1 && t `elem` [Assets, Liabilities]
    then pure <$> createValuationTransaction k (amount * Sum (v1 - v0))
    else return []

createValuationTransaction
  :: (MonadState ValuationState m)
  => (Account, Commodity, Maybe Lot)
  -> Amount
  -> m Command
createValuationTransaction (a, c, l) amount = do
  ValuationState { vsTarget, vsValuationAccount } <- get
  return
    $ TransactionCommand
    $ Transaction Complete "valuation" []
    $ M.fromListM
        [ ((a, c, l)                 , M.singleton vsTarget amount)
        , ((vsValuationAccount, c, l), M.singleton vsTarget (-amount))
        ]
