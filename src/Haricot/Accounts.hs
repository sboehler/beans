module Haricot.Accounts
  ( AccountsException(..)
  , AccountsHistory
  , Accounts
  , Account(..)
  , Holdings
  , Lots
  , mapWithKeys
  , calculateAccounts
  , updateAccounts
  ) where

import           Control.Monad.Catch (Exception, MonadThrow, throwM)
import           Control.Monad.State (evalStateT, get, put)
import           Data.Foldable       (foldlM)
import qualified Data.Map.Strict     as M
import           Data.Scientific     (Scientific)
import           Data.Time.Calendar  (Day)
import           Haricot.AST         (AccountName (..), Balance (..),
                                      Close (..), CommodityName (..), Lot (..),
                                      Open (..), Posting (..), Restriction (..),
                                      Transaction (..), compatibleWith)
import           Haricot.Ledger      (Timestep (..))

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

calculateAccounts ::
     (MonadThrow m, Traversable t) => t Timestep -> m (t Accounts)
calculateAccounts l = evalStateT (mapM f l) mempty
  where
    f ts = get >>= updateAccounts ts >>= put >> get

updateAccounts :: (MonadThrow m) => Timestep -> Accounts -> m Accounts
updateAccounts Timestep {..} accounts =
  foldlM openAccount accounts _openings >>= flip (foldlM closeAccount) _closings >>=
  flip (foldlM checkBalance) _balances >>=
  flip (foldlM bookTransaction) _transactions

openAccount :: (MonadThrow m) => Accounts -> Open -> m Accounts
openAccount accounts open@Open {_account, _restriction} =
  case M.lookup _account accounts of
    Nothing -> return $ M.insert _account (Account _restriction mempty) accounts
    Just _ -> throwM $ AccountIsAlreadyOpen open

closeAccount :: (MonadThrow m) => Accounts -> Close -> m Accounts
closeAccount accounts closing@Close {..} =
  case M.lookup _account accounts of
    Just Account {_holdings}
      | all (all (== 0)) _holdings -> return $ M.delete _account accounts
      | otherwise -> throwM $ BalanceIsNotZero closing
    Nothing -> throwM $ AccountIsNotOpen closing

checkBalance :: (MonadThrow m) => Accounts -> Balance -> m Accounts
checkBalance accounts bal@Balance {_account, _amount, _commodity} =
  case M.lookup _account accounts of
    Nothing -> throwM $ AccountDoesNotExist bal
    Just Account {_holdings} ->
      let amount' = calculateAmount _commodity _holdings
       in if amount' == _amount
            then return accounts
            else throwM (BalanceDoesNotMatch bal amount')

calculateAmount :: CommodityName -> Holdings -> Scientific
calculateAmount commodity = sum . M.findWithDefault M.empty commodity

bookTransaction :: (MonadThrow m) => Accounts -> Transaction -> m Accounts
bookTransaction accounts Transaction {_postings} =
  foldlM bookPosting accounts _postings

bookPosting :: (MonadThrow m) => Accounts -> Posting -> m Accounts
bookPosting accounts p@Posting {_account, _commodity} =
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
