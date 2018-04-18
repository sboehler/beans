module Haricot.Valuation where

import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.Reader.Class (MonadReader, ask, asks)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes)
import           Data.Scientific            (Scientific)
import           Data.Time.Calendar         (Day)
import           Haricot.Accounts
import           Haricot.AST
import           Haricot.Ledger
import           Haricot.Prices

data ValuationContext = VC
  { _target            :: CommodityName
  , _unrealizedAccount :: AccountName
  , _accountsHistory   :: AccountsHistory
  , _pricesHistory     :: PricesHistory
  }

calculateValuation ::
     (MonadThrow m, MonadReader ValuationContext m) => Ledger -> m Ledger
calculateValuation ledger =
  openUnrealizedAccount ledger >>= mapM convertTimestep

openUnrealizedAccount :: (MonadThrow m, MonadReader ValuationContext m) => Ledger -> m Ledger
openUnrealizedAccount ledger = do
  VC {_unrealizedAccount, _target} <- ask
  return $
    case M.lookupMin ledger of
      Just (d, ts@Timestep {_openings}) ->
        let opening =
              Open
                { _pos = Nothing
                , _date = d
                , _account = _unrealizedAccount
                , _restriction = RestrictedTo [_target]
                }
         in M.insert d (ts {_openings = opening : _openings}) ledger
      Nothing -> ledger

convertTimestep ::
     (MonadThrow m, MonadReader ValuationContext m) => Timestep -> m Timestep
convertTimestep ts@Timestep {..} = do
  VC {_pricesHistory, _accountsHistory} <- ask
  let currPrices = lookupLE _date _pricesHistory
      prevPrices = lookupLT _date _pricesHistory
      prevAccounts = lookupLT _date _accountsHistory
  _openings' <- convertOpenings _openings
  _balances' <- convertBalances prevPrices _balances
  _transactions' <- convertTransactions currPrices _transactions
  _valueTransactions <-
    adjustValuationForAccounts _date prevAccounts prevPrices currPrices
  return $
    ts
      { _balances = _balances'
      , _transactions = _transactions' ++ _valueTransactions
      , _openings = _openings'
      }

convertOpenings ::
     (MonadThrow m, MonadReader ValuationContext m) => [Open] -> m [Open]
convertOpenings =
  mapM $ \open@Open {..} ->
    return $ (open :: Open) {_restriction = NoRestriction}

convertBalances ::
     (MonadThrow m, MonadReader ValuationContext m)
  => Prices
  -> [Balance]
  -> m [Balance]
convertBalances p = mapM $ \bal@Balance {..} -> do
  tc <- asks _target
  price <- getPrice p _commodity tc
  let amount = _amount * price
  return $ (bal :: Balance) {_amount = amount, _commodity = tc}

convertTransactions :: (MonadThrow m, MonadReader ValuationContext m) => Prices -> [Transaction] -> m [Transaction]
convertTransactions p = mapM $ \t@Transaction {..} -> do
  postings' <- convertPostings p _postings
  return $ t { _postings = postings'}

convertPostings :: (MonadThrow m, MonadReader ValuationContext m) => Prices -> [Posting] -> m [Posting]
convertPostings prices =
  mapM $ \p@Posting {..} -> do
    tc <- asks _target
    if tc == _commodity
      then return p
      else do
        price <- getPrice prices _commodity tc
        return $ (p :: Posting) {_amount = _amount * price, _commodity = tc}

adjustValuationForAccounts ::
     (MonadThrow m, MonadReader ValuationContext m)
  => Day
  -> Accounts
  -> Prices
  -> Prices
  -> m [Transaction]
adjustValuationForAccounts day accounts p0 p1 =
  if p0 == p1
    then pure []
    else catMaybes <$>
         sequence (mapWithKeys (adjustValuationForAccount day p0 p1) accounts)

adjustValuationForAccount ::
     (MonadThrow m, MonadReader ValuationContext m)
  => Day
  -> Prices
  -> Prices
  -> AccountName
  -> CommodityName
  -> Lot
  -> Scientific
  -> m (Maybe Transaction)
adjustValuationForAccount _date p0 p1 a c l s = do
  VC {_target, _unrealizedAccount} <- ask
  v0 <- getPrice p0 c _target
  v1 <- getPrice p1 c _target
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
                  , _account = _unrealizedAccount
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

lookupLT :: (Monoid v, Ord k) => k -> M.Map k v -> v
lookupLT k m = maybe mempty snd (M.lookupLT k m)

lookupLE :: (Monoid v, Ord k) => k -> M.Map k v -> v
lookupLE k m = maybe mempty snd (M.lookupLE k m)
