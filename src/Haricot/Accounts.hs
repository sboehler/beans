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

import           Control.Monad.Catch (Exception, MonadThrow, throwM)
import           Control.Monad.State (get, put, evalStateT, MonadState, execStateT)
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
calculateAccounts ledger = evalStateT (mapM f ledger) M.empty
  where
    f ts = do
      accounts <- get >>= updateAccounts ts
      put accounts
      return accounts

updateAccounts :: (MonadThrow m) => Timestep -> Accounts -> m Accounts
updateAccounts Timestep {..} =
  execStateT $
  mapM_ openAccount _openings >> mapM_ closeAccount _closings >>
  mapM_ checkBalance _balances >>
  mapM_ bookTransaction _transactions

openAccount :: (MonadThrow m, MonadState Accounts m) => Open -> m ()
openAccount open@Open {_account, _restriction} = do
  accounts <- get
  case M.lookup _account accounts of
    Nothing ->
      put $ M.insert _account (Account _restriction M.empty) accounts
    Just _ -> throwM $ AccountIsAlreadyOpen open

closeAccount :: (MonadThrow m, MonadState Accounts m) => Close -> m ()
closeAccount closing@Close {..} = do
  accounts <- get
  case M.lookup _account accounts of
    Just Account {_holdings}
      | all (all (== 0)) _holdings -> put $ M.delete _account accounts
      | otherwise -> throwM $ BalanceIsNotZero closing
    Nothing -> throwM $ AccountIsNotOpen closing

checkBalance :: (MonadState Accounts m,MonadThrow m) => Balance -> m ()
checkBalance bal@Balance {_account, _amount, _commodity} = do
  accounts <- get
  case M.lookup _account accounts of
    Nothing -> throwM $ AccountDoesNotExist bal
    Just Account {_holdings} ->
      let amount' = calculateAmount _commodity _holdings
       in if amount' /= _amount
            then throwM (BalanceDoesNotMatch bal amount')
            else put accounts

calculateAmount :: CommodityName -> Holdings -> Scientific
calculateAmount commodity = sum . M.findWithDefault M.empty commodity

bookTransaction :: (MonadState Accounts m, MonadThrow m) => Transaction -> m ()
bookTransaction Transaction {_postings} = mapM_ bookPosting _postings

bookPosting :: (MonadState Accounts m, MonadThrow m) => Posting -> m ()
bookPosting p@Posting {_account, _commodity}  = do
  accounts <- get
  case M.lookup _account accounts of
    Just a -> do
      a' <- updateAccount p a
      put $ M.insert _account a' accounts
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
