module Haricot.Accounts
  ( AccountsException(..)
  , AccountsHistory
  , Accounts
  , Restrictions
  , RestrictedAccounts(..)
  , calculateAccounts
  , updateAccounts
  , diffAccounts
  ) where

import           Control.Monad         (unless, when)
import           Control.Monad.Catch   (Exception, MonadThrow, throwM)
import           Control.Monad.State   (MonadState, evalStateT, get, gets, put)
import qualified Data.Map.Merge.Strict as MM
import qualified Data.Map.Strict       as M
import           Data.Scientific       (Scientific)
import           Data.Time.Calendar    (Day)
import           Haricot.AST           (AccountName (..), Balance (..),
                                        Close (..), CommodityName (..),
                                        Lot (..), Open (..), Posting (..),
                                        Restriction (..), Transaction (..),
                                        compatibleWith)
import           Haricot.Ledger        (Timestep (..))

type AccountsHistory = M.Map Day Accounts

type Accounts = M.Map (AccountName, CommodityName, Lot) Scientific
type Restrictions = M.Map AccountName Restriction

data RestrictedAccounts = RestrictedAccounts
  { _accounts     :: Accounts
  , _restrictions :: Restrictions
  }

diffAccounts :: Accounts -> Accounts -> Accounts
diffAccounts = MM.merge MM.preserveMissing
  (MM.mapMissing $ const negate)
  (MM.zipWithMatched $ const (-))

data AccountsException
  = AccountIsNotOpen Close
  | BookingErrorAccountNotOpen Posting
  | BookingErrorCommodityIncompatible Posting Restriction
  | AccountIsAlreadyOpen Open
  | BalanceIsNotZero Close
  | AccountDoesNotExist Balance
  | BalanceDoesNotMatch Balance
                        Scientific
  deriving (Show)

instance Exception AccountsException

calculateAccounts ::
     (MonadThrow m, Traversable t) => t Timestep -> m (t Accounts)
calculateAccounts l = evalStateT (mapM updateAccounts l) (RestrictedAccounts mempty mempty)

updateAccounts :: (MonadThrow m, MonadState RestrictedAccounts m) => Timestep -> m Accounts
updateAccounts Timestep {..} =
  mapM_ openAccount _openings >> mapM_ closeAccount _closings >>
  mapM_ checkBalance _balances >>
  mapM_ bookTransaction _transactions >>
  gets _accounts

openAccount :: (MonadThrow m, MonadState RestrictedAccounts m) => Open -> m ()
openAccount open@Open {_account, _restriction} = do
  RestrictedAccounts {..} <- get
  when (_account `M.member` _restrictions) (throwM $ AccountIsAlreadyOpen open)
  put
    RestrictedAccounts
      {_restrictions = M.insert _account _restriction _restrictions, ..}

closeAccount :: (MonadThrow m, MonadState RestrictedAccounts m) => Close -> m ()
closeAccount closing@Close {..} = do
  RestrictedAccounts {..} <- get
  let (r, restrictions) =
        M.partitionWithKey (const . (== _account)) _restrictions
      (a, accounts) =
        M.partitionWithKey (\(n, _, _) -> const $ n == _account) _accounts
  when (M.null r) (throwM $ AccountIsNotOpen closing)
  unless (all (== 0) a) (throwM $ BalanceIsNotZero closing)
  put RestrictedAccounts {_restrictions = restrictions, _accounts = accounts}

checkBalance :: (MonadThrow m, MonadState RestrictedAccounts m) => Balance -> m ()
checkBalance bal@Balance {_account, _amount, _commodity} = do
  RestrictedAccounts {..} <- get
  unless (M.member _account _restrictions) (throwM $ AccountDoesNotExist bal)
  let s =
        sum
          (M.filterWithKey
             (\(a, c, _) -> const $ a == _account && c == _commodity)
             _accounts)
  unless (s == _amount) (throwM $ BalanceDoesNotMatch bal s)

bookTransaction :: (MonadThrow m, MonadState RestrictedAccounts m) =>  Transaction -> m ()
bookTransaction Transaction {_postings} = mapM_ bookPosting _postings

bookPosting :: (MonadThrow m, MonadState RestrictedAccounts m) => Posting -> m ()
bookPosting p@Posting {_account, _commodity, _amount, _lot} = do
  RestrictedAccounts {..} <- get
  case M.lookup _account _restrictions of
    Nothing -> throwM $ BookingErrorAccountNotOpen p
    Just r -> do
      unless
        (_commodity `compatibleWith` r)
        (throwM $ BookingErrorCommodityIncompatible p r)
      put
        RestrictedAccounts
          { _accounts =
              M.insertWith (+) (_account, _commodity, _lot) _amount _accounts
          , ..
          }
