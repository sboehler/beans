module Haricot.Accounts
  ( AccountsException(..)
  , Accounts
  , Account(..)
  , Holdings
  , Lots
  , calculateAccounts
  ) where

import           Control.Monad       (when)
import           Control.Monad.Catch
import           Data.Foldable       (foldlM, foldrM)
import qualified Data.Map.Strict     as M
import           Data.Scientific
import           Data.Time.Calendar  (Day)
import           Haricot.AST
import           Haricot.Ledger

type AccountsHistory = M.Map Day Accounts

type Accounts = M.Map AccountName Account

data Account = Account {
  _restriction :: Restriction,
  _holdings    :: Holdings
} deriving (Show)

type Holdings = M.Map CommodityName Lots

type Lots = M.Map Lot Scientific

instance Monoid Account where
  mempty = Account NoRestriction M.empty
  Account _ l1 `mappend` Account _ l2 = Account NoRestriction (M.unionWith (M.unionWith (+)) l1 l2)

data AccountsException
  = AccountIsNotOpen Close
  | BookingErrorAccountNotOpen Posting
  | AccountIsAlreadyOpen Open
  | BalanceIsNotZero Close
  | AccountDoesNotExist Balance
  | BalanceDoesNotMatch Balance Scientific
  deriving (Show)

instance Exception AccountsException

calculateAccounts :: (MonadThrow m) => Ledger -> m AccountsHistory
calculateAccounts l = fst <$> foldlM f (M.empty, M.empty) l
  where
    f (accountsHistory, latest) ts@Timestep {_date} = do
      latest' <- updateAccounts ts latest
      return (M.insert _date latest' accountsHistory, latest')

updateAccounts :: (MonadThrow m) => Timestep -> Accounts -> m Accounts
updateAccounts Timestep {..} accounts = do
  accounts' <- foldrM openAccount accounts _openings
  accounts'' <- foldrM closeAccount accounts' _closings
  _ <- mapM_ (checkBalance accounts'') _balances
  foldrM bookTransaction accounts'' _transactions

openAccount :: MonadThrow m => Open -> Accounts -> m Accounts
openAccount open@Open {_account, _restriction} accounts =
  case M.lookup _account accounts of
    Nothing ->
      return $ M.insert _account (Account _restriction M.empty) accounts
    Just _ -> throwM $ AccountIsAlreadyOpen open

closeAccount :: MonadThrow m =>  Close -> Accounts -> m Accounts
closeAccount closing@Close {..} accounts =
  case M.lookup _account accounts of
    Just Account {_holdings}
      | all (all (== 0)) _holdings -> return $ M.delete _account accounts
      | otherwise -> throwM $ BalanceIsNotZero closing
    Nothing -> throwM $ AccountIsNotOpen closing

checkBalance :: MonadThrow m => Accounts -> Balance -> m ()
checkBalance a bal@Balance {_account, _amount, _commodity} =
  case M.lookup _account a of
    Nothing -> throwM $ AccountDoesNotExist bal
    Just Account {_holdings} ->
      let amount' = calculateAmount _holdings _commodity
       in when (amount' /= _amount) $ throwM (BalanceDoesNotMatch bal amount')

calculateAmount :: Holdings -> CommodityName -> Scientific
calculateAmount h c = case M.lookup c h of
  Just m  -> sum m
  Nothing -> 0

bookTransaction :: MonadThrow m => Transaction -> Accounts  -> m Accounts
bookTransaction Transaction{_postings} a = foldlM book a _postings

book :: MonadThrow m => Accounts -> Posting -> m Accounts
book accounts p@Posting {_account, _commodity} =
  case M.lookup _account accounts of
    Just a -> do
      a' <- updateAccount a p
      return $ M.insert _account a' accounts
    _ -> throwM $ BookingErrorAccountNotOpen p

updateAccount :: MonadThrow m => Account -> Posting -> m Account
updateAccount a@Account {_restriction, _holdings} p@Posting {_commodity} =
  if _commodity `compatibleWith` _restriction
    then return $ a {_holdings = updateHoldings _holdings p}
    else throwM $ BookingErrorAccountNotOpen p

updateHoldings :: Holdings -> Posting -> Holdings
updateHoldings h p@Posting {_lot, _amount, _commodity} =
  let lots = M.findWithDefault M.empty _commodity h
   in M.insert _commodity (updateLot lots p) h

updateLot :: Lots -> Posting -> Lots
updateLot lots Posting {_lot, _amount} =
  M.insertWith (+) _lot _amount lots
