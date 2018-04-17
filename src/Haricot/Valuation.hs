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
     (MonadThrow m, MonadReader ValuationContext m)
  => Ledger -> m Ledger
calculateValuation ledger = do
  ledger' <- openUnrealizedAccount ledger
  mapM convertTimestep ledger'

openUnrealizedAccount :: (MonadThrow m, MonadReader ValuationContext m) => Ledger -> m Ledger
openUnrealizedAccount ledger = do
  account <- asks _unrealizedAccount
  target <- asks _target
  case M.lookupMin ledger of
    Just (d, ts@Timestep {_openings}) ->
      let opening =
            Open
              { _pos = Nothing
              , _date = d
              , _account = account
              , _restriction = RestrictedTo [target]
              }
          ledger' = M.insert d (ts {_openings = opening : _openings}) ledger
       in return ledger'
    Nothing -> return ledger -- TODO: throw an exception

convertTimestep ::
     (MonadThrow m, MonadReader ValuationContext m) => Timestep -> m Timestep
convertTimestep ts@Timestep {..}
 = do
  VC {_pricesHistory, _accountsHistory } <- ask
  let currPrices = lookupLE _date _pricesHistory
      prevPrices = lookupLT _date _pricesHistory
      prevAccounts = lookupLT _date _accountsHistory
  _balances' <- convertBalances currPrices _balances
  _transactions' <- convertTransactions currPrices _transactions
  _valueTransactions <- adjustValuationForAccounts _date prevAccounts prevPrices currPrices
  return $ ts {_balances = _balances'}

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
         traverse (adjustValuationForAccount day p0 p1) (flatten accounts)

adjustValuationForAccount ::
     (MonadThrow m, MonadReader ValuationContext m)
  => Day -> Prices
  -> Prices
  -> (AccountName, CommodityName, Lot, Scientific)
  -> m (Maybe Transaction)
adjustValuationForAccount _date p0 p1 (a, c, l, s) = do
  target <- asks _target
  unrealizedAccount <- asks _unrealizedAccount
  v0 <- getPrice p0 c target
  v1 <- getPrice p1 c target
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
                  , _commodity = target
                  , _amount = s * (v1 - v0)
                  , _lot = l
                  }
              , Posting
                  { _pos = Nothing
                  , _account = unrealizedAccount
                  , _commodity = target
                  , _amount = (-s) * (v1 - v0)
                  , _lot = NoLot
                  }
              ]
          }
   in if v0 == v1
        then return Nothing
        else return $ Just t

flatten :: Accounts -> [(AccountName, CommodityName, Lot, Scientific)]
flatten accounts = do
  (name, Account{_holdings}) <- M.toList accounts
  (commodity, lots) <- M.toList _holdings
  (lot, amount) <- M.toList lots
  return (name, commodity, lot, amount)

lookupLT :: (Monoid v, Ord k) => k -> M.Map k v -> v
lookupLT k m = maybe mempty snd (M.lookupLT k m)

lookupLE :: (Monoid v, Ord k) => k -> M.Map k v -> v
lookupLE k m = maybe mempty snd (M.lookupLE k m)
