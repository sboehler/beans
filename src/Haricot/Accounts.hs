module Haricot.Accounts
  ( AccountsException(..)
  , AccountsHistory
  , Accounts
  , Account(..)
  , Holdings
  , Lots
  , calculateAccounts
  ) where

import           Control.Monad.Catch
import           Data.Foldable       (foldlM, foldrM)
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

calculateAccounts :: (MonadThrow m) => Ledger -> m AccountsHistory
calculateAccounts l = fst <$> foldlM f (M.empty, M.empty) l
  where
    f (accountsHistory, accounts) ts@Timestep {_date} = do
      accounts' <- updateAccounts ts accounts
      return (M.insert _date accounts' accountsHistory, accounts')

updateAccounts :: (MonadThrow m) => Timestep -> Accounts -> m Accounts
updateAccounts Timestep {..} accounts =
  openAccounts accounts >>= closeAccounts >>= checkBalances >>= bookTransactions
  where
    openAccounts a = foldrM openAccount a _openings
    closeAccounts a = foldrM closeAccount a _closings
    checkBalances a = foldrM checkBalance a _balances
    bookTransactions a = foldrM bookTransaction a _transactions

openAccount :: MonadThrow m => Open -> Accounts -> m Accounts
openAccount open@Open {_account, _restriction} accounts =
  case M.lookup _account accounts of
    Nothing ->
      return $ M.insert _account (Account _restriction M.empty) accounts
    Just _ -> throwM $ AccountIsAlreadyOpen open

closeAccount :: MonadThrow m => Close -> Accounts -> m Accounts
closeAccount closing@Close {..} accounts =
  case M.lookup _account accounts of
    Just Account {_holdings}
      | all (all (== 0)) _holdings -> return $ M.delete _account accounts
      | otherwise -> throwM $ BalanceIsNotZero closing
    Nothing -> throwM $ AccountIsNotOpen closing

checkBalance :: MonadThrow m => Balance -> Accounts -> m Accounts
checkBalance bal@Balance {_account, _amount, _commodity} accounts =
  case M.lookup _account accounts of
    Nothing -> throwM $ AccountDoesNotExist bal
    Just Account {_holdings} ->
      let amount' = calculateAmount _commodity _holdings
       in if amount' /= _amount
            then throwM (BalanceDoesNotMatch bal amount')
            else return accounts

calculateAmount :: CommodityName -> Holdings -> Scientific
calculateAmount commodity = sum . M.findWithDefault M.empty commodity

bookTransaction :: MonadThrow m => Transaction -> Accounts -> m Accounts
bookTransaction Transaction {_postings} accounts =
  foldrM bookPosting accounts _postings

bookPosting :: MonadThrow m => Posting -> Accounts -> m Accounts
bookPosting p@Posting {_account, _commodity} accounts =
  case M.lookup _account accounts of
    Just a -> do
      a' <- updateAccount p a
      return $ M.insert _account a' accounts
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
