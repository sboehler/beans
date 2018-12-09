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
                                                , foldM
                                                , when
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
  accounts' <- foldM process accounts (mconcat $ M.elems previous)
  return (accounts', later)

check :: (MonadThrow m) => Restrictions -> Command -> m Restrictions
check restrictions (CmdOpen open@Open { _openAccount, _openRestriction }) = do
  when (_openAccount `M.member` restrictions)
       (throwM $ AccountIsAlreadyOpen open)
  return $ M.insert _openAccount _openRestriction restrictions
check restrictions (CmdClose closing@Close { _closeAccount }) = do
  let (r, remainingRestrictions) =
        M.partitionWithKey (const . (== _closeAccount)) restrictions
  when (null r) (throwM $ AccountIsNotOpen closing)
  return remainingRestrictions
check restrictions (CmdTransaction Transaction { _transactionPostings }) = do
  mapM_ (g restrictions) $ M.toList _transactionPostings
  return restrictions
 where
  g r (Position { _positionAccount, _positionCommodity }, s) =
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
check restrictions (CmdBalance bal@Balance { _balanceAccount }) = do
  unless (_balanceAccount `M.member` restrictions)
    $ throwM (AccountDoesNotExist bal)
  return restrictions
check restrictions _ = pure restrictions

process :: (MonadThrow m) => Accounts -> Command -> m Accounts
process accounts (CmdClose command@Close { _closeAccount }) = do
  let (deletedAccount, remainingAccounts) =
        M.partitionKeys ((== _closeAccount) . _positionAccount) accounts
  unless ((all . all) (== 0) deletedAccount) (throwM $ BalanceIsNotZero command)
  return remainingAccounts
process accounts (CmdTransaction Transaction { _transactionPostings }) =
  return $ accounts `mappend` _transactionPostings
process accounts (CmdBalance bal@Balance { _balanceAccount, _balanceAmount, _balanceCommodity })
  = do
    let s = balance _balanceAccount _balanceCommodity accounts
    unless (s == _balanceAmount) (throwM $ BalanceDoesNotMatch bal s)
    return accounts
process accounts _ = pure accounts
