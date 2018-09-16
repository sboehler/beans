module Beans.Accounts
  ( AccountsException(..)
  , Accounts
  , State(..)
  , calculateAccounts
  , updateAccounts
  ) where

import           Beans.Data.Accounts     (Accounts, Amount, Posting (..))
import qualified Beans.Data.Accounts     as A
import           Beans.Data.Directives   (Balance (..), Close (..),
                                          Command (..), Open (..),
                                          Transaction (..))
import qualified Beans.Data.Map          as M
import           Beans.Data.Restrictions (Restriction, Restrictions)
import qualified Beans.Data.Restrictions as R
import           Beans.Ledger            (Ledger, Timestep (..), toList)
import           Control.Monad           (unless, when)
import           Control.Monad.Catch     (Exception, MonadThrow, throwM)
import           Control.Monad.State     (MonadState, evalStateT, get, gets,
                                          modify)
import           Data.Time.Calendar      (Day)

data State = S
  { sAccounts     :: Accounts
  , sRestrictions :: Restrictions
  , sCheck        :: Bool
  }

data AccountsException
  = AccountIsNotOpen Close
  | BookingErrorAccountNotOpen Posting
  | BookingErrorCommodityIncompatible Posting
                                      Restriction
  | AccountIsAlreadyOpen Open
  | BalanceIsNotZero Close
  | AccountDoesNotExist Balance
  | BalanceDoesNotMatch Balance
                        Amount
  deriving (Show)

instance Exception AccountsException

calculateAccounts :: (MonadThrow m) => Bool -> Ledger -> m (M.Map Day Accounts)
calculateAccounts check l =
  let computation = M.fromList <$> mapM updateAccounts (toList l)
   in evalStateT computation (S mempty mempty check)

updateAccounts ::
     (MonadThrow m, MonadState State m) => Timestep -> m (Day, Accounts)
updateAccounts (Timestep day commands) = do
  a <- mapM_ process commands >> gets sAccounts
  return (day, a)

process :: (MonadThrow m, MonadState State m) => Command -> m ()
process (OpenCommand open@Open {oAccount, oRestriction}) = do
  S {sRestrictions} <- get
  when (R.isOpen oAccount sRestrictions) (throwM $ AccountIsAlreadyOpen open)
  modify (\s -> s {sRestrictions = R.add oAccount oRestriction sRestrictions})
process (CloseCommand closing@Close {cAccount}) = do
  (restriction, remainingRestrictions) <-
    R.split cAccount <$> gets sRestrictions
  when (R.isEmpty restriction) (throwM $ AccountIsNotOpen closing)
  (deletedAccount, remainingAccounts) <- A.split cAccount <$> gets sAccounts
  unless (all (== 0) deletedAccount) (throwM $ BalanceIsNotZero closing)
  modify
    (\s ->
       s {sRestrictions = remainingRestrictions, sAccounts = remainingAccounts})
process (BalanceCommand bal@Balance {bAccount, bAmount, bCommodity}) = do
  check <- gets sCheck
  when check $ do
    r <- gets sRestrictions
    unless (R.isOpen bAccount r) (throwM $ AccountDoesNotExist bal)
    s <- A.balance bAccount bCommodity <$> gets sAccounts
    unless (s == bAmount) (throwM $ BalanceDoesNotMatch bal s)
process (TransactionCommand Transaction {tPostings}) =
  mapM_ bookPosting tPostings
process _ = pure ()

bookPosting :: (MonadThrow m, MonadState State m) => Posting -> m ()
bookPosting p@Posting {pAccount, pCommodity} = do
  S {sRestrictions, sAccounts} <- get
  case R.find pAccount sRestrictions of
    Nothing -> throwM $ BookingErrorAccountNotOpen p
    Just r -> do
      unless
        (R.isCompatible r pCommodity)
        (throwM $ BookingErrorCommodityIncompatible p r)
      modify
        (\s -> s {sAccounts = A.book p sAccounts})
