module Haricot.Accounts
  ( AccountsException(..)
  , Accounts
  , book
  , updateAccounts
  ) where

import           Control.Monad.Catch
import           Data.Foldable       (foldrM)
import qualified Data.Map.Strict     as M
import           Data.Scientific
import           Haricot.AST
import           Haricot.Ledger

type Accounts = M.Map AccountName Account

data Account = Account {
  _restriction :: Restriction,
  _holdings    :: Holdings
} deriving (Show)

type Holdings = M.Map CommodityName Lots

type Lots = M.Map (Maybe Lot) Scientific

instance Monoid Account where
  mempty = Account NoRestriction M.empty
  Account _ l1 `mappend` Account _ l2 = Account NoRestriction (M.unionWith (M.unionWith (+)) l1 l2)

data AccountsException
  = AccountIsNotOpen Close
  | BookingErrorAccountNotOpen CompletePosting
  | AccountIsAlreadyOpen Open
  | BalanceIsNotZero Close
  deriving (Show)

instance Exception AccountsException


updateAccounts :: (MonadThrow m) => Accounts -> Timestep -> m Accounts
updateAccounts accounts Timestep {..} = do
  accounts' <- foldrM openAccount accounts _openings
  foldrM closeAccount accounts' _closings

openAccount :: MonadThrow m => Open -> Accounts -> m Accounts
openAccount open@Open {_account, _restriction} accounts =
  case M.lookup _account accounts of
    Nothing ->
      return $ M.insert _account (Account _restriction M.empty) accounts
    Just _ -> throwM $ AccountIsAlreadyOpen open

closeAccount :: MonadThrow m =>  Close -> Accounts -> m Accounts
closeAccount closing@Close {..} accounts =
  case M.lookup _account accounts of
    Just Account { _holdings }
      | all (all (== 0)) _holdings -> return $ M.delete _account accounts
      | otherwise                 -> throwM $ BalanceIsNotZero closing
    Nothing                       -> throwM $ AccountIsNotOpen closing


book :: (MonadThrow m) => Accounts -> CompletePosting -> m Accounts
book accounts p@CompletePosting {_account, _commodity} =
  case M.lookup _account accounts of
    Just a ->
      updateAccount a p >>= \a' -> return $ M.insert _account a' accounts
    _ -> throwM $ BookingErrorAccountNotOpen p

updateAccount :: MonadThrow m => Account -> CompletePosting -> m Account
updateAccount a@Account {_restriction, _holdings} p@CompletePosting {_commodity} =
  if _commodity `compatibleWith` _restriction
    then return $ a {_holdings = updateHoldings _holdings p}
    else throwM $ BookingErrorAccountNotOpen p

updateHoldings :: Holdings -> CompletePosting -> Holdings
updateHoldings h p@CompletePosting {_lot, _amount, _commodity} =
  let lots = M.findWithDefault M.empty _commodity h
   in M.insert _commodity (updateLot lots p) h

updateLot :: Lots -> CompletePosting -> Lots
updateLot lots CompletePosting {_lot, _amount} =
  M.insertWith (+) _lot _amount lots
