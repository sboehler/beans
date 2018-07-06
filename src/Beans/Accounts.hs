module Beans.Accounts
  ( AccountsException(..)
  , Accounts
  , RestrictedAccounts(..)
  , calculateAccounts
  , updateAccounts
  ) where

import           Beans.Data.Accounts     (Accounts, Amount, add, balance, split)
import           Beans.Data.Directives   (Balance (..), Close (..), Open (..),
                                          Posting (..), Transaction (..))
import           Beans.Data.Restrictions (Restriction, Restrictions)
import qualified Beans.Data.Restrictions as R
import           Beans.Ledger            (Timestep (..))
import           Control.Monad           (unless, when)
import           Control.Monad.Catch     (Exception, MonadThrow, throwM)
import           Control.Monad.State     (MonadState, evalStateT, get, gets,
                                          put)


data RestrictedAccounts = RestrictedAccounts
  { _accounts     :: Accounts
  , _restrictions :: Restrictions
  }

data AccountsException
  = AccountIsNotOpen Close
  | BookingErrorAccountNotOpen Posting
  | BookingErrorCommodityIncompatible Posting Restriction
  | AccountIsAlreadyOpen Open
  | BalanceIsNotZero Close
  | AccountDoesNotExist Balance
  | BalanceDoesNotMatch Balance
                        Amount
  deriving (Show)

instance Exception AccountsException

calculateAccounts ::
     (MonadThrow m, Traversable t) => Bool -> t Timestep -> m (t Accounts)
calculateAccounts check l = evalStateT (mapM (updateAccounts check) l) (RestrictedAccounts mempty mempty)

updateAccounts :: (MonadThrow m, MonadState RestrictedAccounts m) => Bool -> Timestep -> m Accounts
updateAccounts check Timestep {..} =
  mapM_ openAccount _openings >> mapM_ closeAccount _closings >>
  when check (mapM_ checkBalance _balances) >>
  mapM_ bookTransaction _transactions >>
  gets _accounts

openAccount :: (MonadThrow m, MonadState RestrictedAccounts m) => Open -> m ()
openAccount open@Open {_account, _restriction} = do
  RestrictedAccounts {..} <- get
  when (_account `R.isOpen` _restrictions) (throwM $ AccountIsAlreadyOpen open)
  put
    RestrictedAccounts
      {_restrictions = R.add _account _restriction _restrictions, ..}

closeAccount :: (MonadThrow m, MonadState RestrictedAccounts m) => Close -> m ()
closeAccount closing@Close {..} = do
  RestrictedAccounts {..} <- get
  let (r, rs) = R.split _account _restrictions
  when (R.isEmpty r) (throwM $ AccountIsNotOpen closing)
  let (deleted, remaining) = split _account _accounts
  unless (all (== 0) deleted) (throwM $ BalanceIsNotZero closing)
  put RestrictedAccounts {_restrictions = rs, _accounts = remaining}

checkBalance :: (MonadThrow m, MonadState RestrictedAccounts m) => Balance -> m ()
checkBalance bal@Balance {_account, _amount, _commodity} = do
  RestrictedAccounts {..} <- get
  unless (R.isOpen _account _restrictions) (throwM $ AccountDoesNotExist bal)
  let s = balance _account _commodity _accounts
  unless (s == _amount) (throwM $ BalanceDoesNotMatch bal s)

bookTransaction :: (MonadThrow m, MonadState RestrictedAccounts m) =>  Transaction -> m ()
bookTransaction Transaction {_postings} = mapM_ bookPosting _postings

bookPosting :: (MonadThrow m, MonadState RestrictedAccounts m) => Posting -> m ()
bookPosting p@Posting {_account, _commodity, _amount, _lot} = do
  RestrictedAccounts {..} <- get
  case R.find _account _restrictions of
    Nothing -> throwM $ BookingErrorAccountNotOpen p
    Just r -> do
      unless
        (R.isCompatible r _commodity)
        (throwM $ BookingErrorCommodityIncompatible p r)
      put
        RestrictedAccounts
          {_accounts = add _account _commodity _lot _amount _accounts, ..}
