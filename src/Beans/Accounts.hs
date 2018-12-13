module Beans.Accounts
  ( Accounts
  , sumUntil
  , check
  , process
  )
where

import           Beans.Model
import           Data.Monoid                    ( mconcat )
import qualified Beans.Data.Map                as M
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.State            ( MonadState
                                                , get
                                                , gets
                                                , put
                                                , modify
                                                , evalStateT
                                                )
import           Control.Monad.Catch            ( Exception
                                                , MonadThrow
                                                , throwM
                                                )

data AccountsException
  = AccountIsNotOpen Close
  | BookingErrorAccountNotOpen Account
  | BookingErrorCommodityIncompatible Account
                                      Commodity
                                      Amount
                                      Restriction
  | AccountIsAlreadyOpen Open
  | BalanceIsNotZero Close
  | AccountDoesNotExist Balance
  | BalanceDoesNotMatch Balance
                        Amount
  deriving (Show)

instance Exception AccountsException

sumUntil :: (MonadThrow m) => Date -> Ledger -> Accounts -> m (Accounts, Ledger)
sumUntil date ledger accounts = do
  let (previous, later) = M.partitionWithKey (const . (<= date)) ledger
  accounts' <- evalStateT (mapM_ process (mconcat $ M.elems previous) >> get)
                          accounts
  return (accounts', later)

check :: (MonadState Restrictions m, MonadThrow m) => Command -> m ()
check (CmdOpen open@Open { _openAccount, _openRestriction }) = do
  restrictions <- get
  when (_openAccount `M.member` restrictions)
       (throwM $ AccountIsAlreadyOpen open)
  put $ M.insert _openAccount _openRestriction restrictions
check (CmdClose closing@Close { _closeAccount }) = do
  (r, remainingRestrictions) <- gets
    $ M.partitionWithKey (const . (== _closeAccount))
  when (null r) (throwM $ AccountIsNotOpen closing)
  put remainingRestrictions
check (CmdTransaction Transaction { _transactionPostings }) =
  mapM_ g (M.toList _transactionPostings)
 where
  g (Position { _positionAccount, _positionCommodity }, s) = do
    r <- get
    case M.lookup _positionAccount r of
      Nothing -> throwM $ BookingErrorAccountNotOpen _positionAccount
      Just r' -> unless
        (isCompatible r' _positionCommodity)
        (throwM $ BookingErrorCommodityIncompatible
          _positionAccount
          _positionCommodity
          (M.findWithDefaultM _positionCommodity s)
          r'
        )
check (CmdBalance bal@Balance { _balanceAccount }) = do
  restrictions <- get
  unless (_balanceAccount `M.member` restrictions)
    $ throwM (AccountDoesNotExist bal)
check _ = pure ()

process :: (MonadState Accounts m, MonadThrow m) => Command -> m ()
process (CmdClose command@Close { _closeAccount }) = do
  (deletedAccounts, remainingAccounts) <-
    gets $ M.partitionKeys ((== _closeAccount) . _positionAccount)
  unless ((all . all) (== 0) deletedAccounts)
         (throwM $ BalanceIsNotZero command)
  put remainingAccounts
process (CmdTransaction Transaction { _transactionPostings }) =
  modify (mappend _transactionPostings)
process (CmdBalance bal@Balance { _balanceAccount, _balanceAmount, _balanceCommodity })
  = do
    s <- gets $ balance _balanceAccount _balanceCommodity
    unless (s == _balanceAmount) (throwM $ BalanceDoesNotMatch bal s)
process _ = pure ()
