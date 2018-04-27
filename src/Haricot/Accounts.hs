module Haricot.Accounts
  ( AccountsException(..)
  , AccountsHistory
  , Accounts
  , Account(..)
  , Holdings
  , Lots
  , mapWithKeys
  , calculateAccounts
  ) where

import           Control.Monad       (when)
import           Control.Monad.Catch (Exception, MonadThrow, throwM)
import           Control.Monad.State (MonadState, evalStateT, get, gets, modify)
import qualified Data.Map.Strict     as M
import           Data.Scientific
import           Data.Time.Calendar  (Day)
import           Haricot.AST
import           Haricot.Ledger

type AccountsHistory = M.Map Day Accounts

type Accounts = M.Map AccountName Account

data Account = Account
  { _restriction :: Restriction
  , _holdings    :: Holdings
  }

instance Show Account where
  show (Account _ h) = show h

instance Monoid Account where
  mempty = Account mempty mempty
  Account r1 l1 `mappend` Account r2 l2 =
    Account (r1 `mappend` r2) (M.unionWith (M.unionWith (+)) l1 l2)

type Holdings = M.Map CommodityName Lots

type Lots = M.Map Lot Scientific

data AccountsException
  = AccountIsNotOpen Close
  | BookingErrorAccountNotOpen Posting
  | AccountIsAlreadyOpen Open
  | BalanceIsNotZero Close
  | AccountDoesNotExist Balance
  | BalanceDoesNotMatch Balance
                        Scientific
  deriving (Show)

instance Exception AccountsException

mapWithKeys ::
     (AccountName -> CommodityName -> Lot -> Scientific -> a) -> Accounts -> [a]
mapWithKeys f accounts = do
  (name, Account {_holdings}) <- M.toList accounts
  (commodity, lots) <- M.toList _holdings
  (lot, amount) <- M.toList lots
  return $ f name commodity lot amount

calculateAccounts :: (MonadThrow m, Traversable t) => t Timestep -> m (t Accounts)
calculateAccounts ledger = evalStateT (mapM updateAccounts ledger) mempty

updateAccounts :: (MonadState Accounts m, MonadThrow m) => Timestep -> m Accounts
updateAccounts Timestep {..} =
  mapM_ openAccount _openings >> mapM_ closeAccount _closings >>
  mapM_ checkBalance _balances >>
  mapM_ bookTransaction _transactions >> get

openAccount :: (MonadThrow m, MonadState Accounts m) => Open -> m ()
openAccount open@Open {_account, _restriction} =
  gets (M.lookup _account) >>= \case
    Nothing -> modify $ M.insert _account (Account _restriction mempty)
    Just _ -> throwM $ AccountIsAlreadyOpen open

closeAccount :: (MonadThrow m, MonadState Accounts m) => Close -> m ()
closeAccount closing@Close {..} =
  gets (M.lookup _account) >>= \case
    Just Account {_holdings}
      | all (all (== 0)) _holdings -> modify $ M.delete _account
      | otherwise -> throwM $ BalanceIsNotZero closing
    Nothing -> throwM $ AccountIsNotOpen closing

checkBalance :: (MonadState Accounts m,MonadThrow m) => Balance -> m ()
checkBalance bal@Balance {_account, _amount, _commodity} =
  gets (M.lookup _account) >>= \case
    Nothing -> throwM $ AccountDoesNotExist bal
    Just Account {_holdings} ->
      let amount' = calculateAmount _commodity _holdings
       in when (amount' /= _amount) (throwM (BalanceDoesNotMatch bal amount'))

calculateAmount :: CommodityName -> Holdings -> Scientific
calculateAmount commodity = sum . M.findWithDefault M.empty commodity

bookTransaction :: (MonadState Accounts m, MonadThrow m) => Transaction -> m ()
bookTransaction Transaction {_postings} = mapM_ bookPosting _postings

bookPosting :: (MonadState Accounts m, MonadThrow m) => Posting -> m ()
bookPosting p@Posting {_account, _commodity} = gets (M.lookup _account) >>= \case
    Just a -> updateAccount p a >>= modify . M.insert _account
    _ -> throwM $ BookingErrorAccountNotOpen p

updateAccount :: MonadThrow m => Posting -> Account -> m Account
updateAccount p@Posting {_commodity} a@Account {_restriction, _holdings} =
  if _commodity `compatibleWith` _restriction
    then return $ a {_holdings = updateHoldings p _holdings}
    else throwM $ BookingErrorAccountNotOpen p

updateHoldings :: Posting -> Holdings -> Holdings
updateHoldings p@Posting {_lot, _amount, _commodity} h =
  let lots = M.findWithDefault M.empty _commodity h
   in M.insert _commodity (updateLot p lots) h

updateLot :: Posting -> Lots -> Lots
updateLot Posting {_lot, _amount} = M.insertWith (+) _lot _amount
