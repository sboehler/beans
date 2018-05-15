module Haricot.Valuation where

import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.Reader.Class (MonadReader, ask, asks)
import           Control.Monad.State        (MonadState, evalStateT, get, put)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes)
import           Data.Scientific            (Scientific)
import           Data.Time.Calendar         (Day)
import           Haricot.Accounts           (Accounts, mapWithKeys,
                                             updateAccounts)
import           Haricot.AST                (AccountName (..), Balance (..),
                                             CommodityName (..), Flag (..),
                                             Lot (..), Open (..), Posting (..),
                                             Restriction (..), Transaction (..))
import           Haricot.Ledger             (Ledger, Timestep (..))
import           Haricot.Prices             (NormalizedPrices, Prices,
                                             lookupPrice, normalize,
                                             updatePrices)

data Config = Config
  { _target           :: CommodityName
  , _valuationAccount :: AccountName
  }

data ValuationState = ValuationState
  { _prices           :: Prices
  , _normalizedPrices :: NormalizedPrices
  , _accounts         :: Accounts
  }

calculateValuation :: MonadThrow m => CommodityName -> AccountName -> Ledger -> m Ledger
calculateValuation _target _valuationAccount ledger =
  runReaderT
    (evalStateT
       (mapM convertTimestep ledger)
       (ValuationState mempty mempty mempty))
    Config {..}

convertTimestep ::
     (MonadThrow m, MonadReader Config m, MonadState ValuationState m)
  => Timestep
  -> m Timestep
convertTimestep ts@Timestep {..} = do
  Config {_target} <- ask
  ValuationState {_prices, _accounts, _normalizedPrices} <- get
  _accounts' <- updateAccounts ts _accounts
  let _prices' = updatePrices ts _prices
      _normalizedPrices' = normalize _prices' _target
  put
    ValuationState
      { _prices = _prices'
      , _normalizedPrices = _normalizedPrices'
      , _accounts = _accounts'
      }
  _openings' <- convertOpenings _openings
  _balances' <- convertBalances _normalizedPrices _balances
  _transactions' <- convertTransactions _normalizedPrices' _transactions
  _valueTransactions <-
    adjustValuationForAccounts
      _date
      _accounts
      _normalizedPrices
      _normalizedPrices'
  return $
    ts
      { _balances = _balances'
      , _transactions = _transactions' ++ _valueTransactions
      , _openings = _openings'
      }

convertOpenings :: (MonadThrow m, MonadReader Config m) => [Open] -> m [Open]
convertOpenings =
  mapM $ \open@Open {..} ->
    return $ (open :: Open) {_restriction = NoRestriction}

convertBalances ::
     (MonadThrow m, MonadReader Config m)
  => NormalizedPrices
  -> [Balance]
  -> m [Balance]
convertBalances p =
  mapM $ \bal@Balance {..} -> do
    tc <- asks _target
    price <- lookupPrice _commodity p
    let amount = _amount * price
    return $ (bal :: Balance) {_amount = amount, _commodity = tc}

convertTransactions ::
     (MonadThrow m, MonadReader Config m)
  => NormalizedPrices
  -> [Transaction]
  -> m [Transaction]
convertTransactions p =
  mapM $ \t@Transaction {..} -> do
    _account <- asks _valuationAccount
    postings' <- convertPostings p _postings
    let imbalances = calculateImbalances postings'
        balancePostings =
          map
            (\(c, a) ->
               Posting
                 { _pos = Nothing
                 , _amount = negate a
                 , _commodity = c
                 , _account = _account
                 , _lot = NoLot
                 })
            imbalances
    return $ t {_postings = postings' ++ balancePostings}


calculateImbalances :: [Posting] -> [(CommodityName, Scientific)]
calculateImbalances =
  M.toList . M.filter ((> 0.005) . abs) . M.fromListWith (+) . fmap weight
  where
    weight Posting {..} = (_commodity, _amount)

convertPostings ::
     (MonadThrow m, MonadReader Config m)
  => NormalizedPrices
  -> [Posting]
  -> m [Posting]
convertPostings prices =
  mapM $ \p@Posting {..} -> do
    tc <- asks _target
    if tc == _commodity
      then return p
      else do
        price <- lookupPrice _commodity prices
        return $ (p :: Posting) {_amount = _amount * price, _commodity = tc}

adjustValuationForAccounts ::
     (MonadThrow m, MonadReader Config m)
  => Day
  -> Accounts
  -> NormalizedPrices
  -> NormalizedPrices
  -> m [Transaction]
adjustValuationForAccounts day accounts p0 p1 =
  if p0 == p1
    then pure []
    else catMaybes <$>
         sequence (mapWithKeys (adjustValuationForAccount day p0 p1) accounts)

adjustValuationForAccount ::
     (MonadThrow m, MonadReader Config m)
  => Day
  -> NormalizedPrices
  -> NormalizedPrices
  -> AccountName
  -> CommodityName
  -> Lot
  -> Scientific
  -> m (Maybe Transaction)
adjustValuationForAccount _date p0 p1 a c l s = do
  Config {_target, _valuationAccount} <- ask
  v0 <- lookupPrice c p0
  v1 <- lookupPrice c p1
  let t =
        Transaction
          { _pos = Nothing
          , _date
          , _flag = Complete
          , _description = "Unrealized"
          , _tags = []
          , _postings =
              [ Posting
                  { _pos = Nothing
                  , _account = a
                  , _commodity = _target
                  , _amount = s * (v1 - v0)
                  , _lot = l
                  }
              , Posting
                  { _pos = Nothing
                  , _account = _valuationAccount
                  , _commodity = _target
                  , _amount = (-s) * (v1 - v0)
                  , _lot = NoLot
                  }
              ]
          }
   in return $
      if v0 == v1
        then Nothing
        else Just t
