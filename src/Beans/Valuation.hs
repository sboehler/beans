module Beans.Valuation where

import           Beans.Accounts          (RestrictedAccounts (..),
                                          updateAccounts)
import           Beans.Data.Accounts     (AccountName (..), AccountType (..),
                                          Accounts, Amount, CommodityName (..),
                                          Lot (..), toList)
import           Beans.Data.Directives   (Flag (..), Open (..), Posting (..),
                                          Transaction (..))
import qualified Beans.Data.Map          as M
import           Beans.Data.Restrictions (Restriction (..), Restrictions)
import           Beans.Ledger            (Ledger, Timestep (..))
import           Beans.Prices            (NormalizedPrices, Prices, lookupPrice,
                                          normalize, updatePrices)
import           Control.Monad.Catch     (MonadThrow)
import           Control.Monad.State     (MonadState, evalStateT, execStateT,
                                          get, gets, put)
import           Data.Maybe              (catMaybes)
import           Data.Monoid             (Sum (Sum))
import           Data.Time.Calendar      (Day, fromGregorian)

data ValuationState = ValuationState
  { _prices               :: Prices
  , _prevNormalizedPrices :: NormalizedPrices
  , _normalizedPrices     :: NormalizedPrices
  , _prevAccounts         :: Accounts
  , _accounts             :: Accounts
  , _restrictions         :: Restrictions
  , _target               :: CommodityName
  , _valuationAccount     :: AccountName
  , _date                 :: Day
  }

calculateValuation :: MonadThrow m => CommodityName -> AccountName -> Ledger -> m Ledger
calculateValuation _target _valuationAccount ledger =
  evalStateT
     (mapM convertTimestep ledger)
     ValuationState
        { _prices = mempty
        , _prevNormalizedPrices = mempty
        , _normalizedPrices = mempty
        , _prevAccounts = mempty
        , _accounts = mempty
        , _restrictions = mempty
        , _date = fromGregorian 1900 1 1
        , ..
        }

convertTimestep ::
     (MonadThrow m, MonadState ValuationState m)
  => Timestep
  -> m Timestep
convertTimestep timestep@Timestep {..} = do
  ValuationState {..} <- get
  RestrictedAccounts _accounts' _restrictions' <-
    execStateT
      (updateAccounts timestep)
      (RestrictedAccounts _accounts _restrictions)
  let _prices' = updatePrices timestep _prices
  put
    ValuationState
      { _prices = _prices'
      , _prevNormalizedPrices = _normalizedPrices
      , _normalizedPrices = normalize _prices' _target
      , _prevAccounts = _accounts
      , _accounts = _accounts'
      , _restrictions = _restrictions'
      , ..
      }
  convertedOpenings <- mapM convertOpening _openings
  convertedTransactions <- mapM convertTransaction _transactions
  valuationTransactions <- adjustValuationForAccounts
  return $
    timestep
      { _balances = []
      , _transactions = convertedTransactions ++ valuationTransactions
      , _openings = convertedOpenings
      }

convertOpening :: MonadState ValuationState m => Open -> m Open
convertOpening o = do
  t <- gets _target
  return $ o {_restriction = RestrictedTo [t]}

convertTransaction ::
     (MonadThrow m, MonadState ValuationState m)
  => Transaction
  -> m Transaction
convertTransaction Transaction {..} = do
  ValuationState { _valuationAccount, _normalizedPrices } <- get
  postings <- mapM (convertPosting _normalizedPrices) _postings
  let balancePostings = map (f _valuationAccount) (calculateImbalances postings)
  return Transaction {_postings = postings ++ balancePostings, ..}
  where
    f _account (_commodity, amount) =
      Posting {_pos = Nothing, _amount = negate amount, _lot = Nothing, ..}


calculateImbalances :: [Posting] -> [(CommodityName, Amount)]
calculateImbalances =
  M.toList . M.filter ((> Sum 0.005) . abs) . M.fromList . fmap weight
  where
    weight Posting {..} = (_commodity, _amount)

convertPosting ::
     (MonadThrow m, MonadState ValuationState m)
  => NormalizedPrices
  -> Posting
  -> m Posting
convertPosting prices Posting {..} = do
  tc <- gets _target
  if tc == _commodity
    then return Posting {..}
    else do
      price <- lookupPrice _commodity prices
      return Posting {_amount = _amount * Sum price, _commodity = tc, ..}

adjustValuationForAccounts ::
     (MonadThrow m, MonadState ValuationState m)
  => m [Transaction]
adjustValuationForAccounts = do
  ValuationState {_prevAccounts, _date} <- get
  s <- sequence $ adjustValuationForAccount <$> toList _prevAccounts
  return $ catMaybes s

adjustValuationForAccount ::
     (MonadThrow m, MonadState ValuationState m)
  => ((AccountName, CommodityName, Maybe Lot), Amount)
  -> m (Maybe Transaction)
adjustValuationForAccount ((a@(AccountName t _), c, l), s) = do
  ValuationState { _target
                 , _date
                 , _valuationAccount
                 , _prevNormalizedPrices
                 , _normalizedPrices
                 } <- get
  v0 <- lookupPrice c _prevNormalizedPrices
  v1 <- lookupPrice c _normalizedPrices
  if v0 /= v1 && t `elem` [Assets, Liabilities]
    then Just <$> createValuationTransaction a l (s * Sum (v1 - v0))
    else return Nothing

createValuationTransaction ::
     MonadState ValuationState m
  => AccountName
  -> Maybe Lot
  -> Amount
  -> m Transaction
createValuationTransaction _account _lot _amount = do
  ValuationState {_target, _date, _valuationAccount} <- get
  return
    Transaction
      { _pos = Nothing
      , _flag = Complete
      , _description = "valuation"
      , _tags = []
      , _date
      , _postings =
          [ Posting
              {_pos = Nothing, _account, _commodity = _target, _amount, _lot}
          , Posting
              { _pos = Nothing
              , _account = _valuationAccount
              , _commodity = _target
              , _amount = -_amount
              , _lot = Nothing
              }
          ]
      }
