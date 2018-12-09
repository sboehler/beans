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
check restrictions (CmdOpen open@Open { oAccount, oRestriction }) = do
  when (oAccount `M.member` restrictions) (throwM $ AccountIsAlreadyOpen open)
  return $ M.insert oAccount oRestriction restrictions
check restrictions (CmdClose closing@Close { cAccount }) = do
  let (restriction, remainingRestrictions) =
        M.partitionWithKey (const . (== cAccount)) restrictions
  when (null restriction) (throwM $ AccountIsNotOpen closing)
  return remainingRestrictions
check restrictions (CmdTransaction Transaction { tPostings }) = do
  mapM_ (g restrictions) $ M.toList tPostings
  return restrictions
 where
  g r (Position { pAccount, pCommodity }, s) = case M.lookup pAccount r of
    Nothing -> throwM $ BookingErrorAccountNotOpen pAccount
    Just r' -> unless
      (isCompatible r' pCommodity)
      (throwM $ BookingErrorCommodityIncompatible
        pAccount
        pCommodity
        (M.findWithDefaultM pCommodity s)
        r'
      )
check restrictions (CmdBalance bal@Balance { bAccount }) = do
  unless (bAccount `M.member` restrictions) $ throwM (AccountDoesNotExist bal)
  return restrictions
check restrictions _ = pure restrictions

process :: (MonadThrow m) => Accounts -> Command -> m Accounts
process accounts (CmdClose command@Close { cAccount }) = do
  let (deletedAccount, remainingAccounts) =
        M.partitionKeys ((== cAccount) . pAccount) accounts
  unless ((all . all) (== 0) deletedAccount) (throwM $ BalanceIsNotZero command)
  return remainingAccounts
process accounts (CmdTransaction Transaction { tPostings }) =
  return $ accounts `mappend` tPostings
process accounts (CmdBalance bal@Balance { bAccount, bAmount, bCommodity }) =
  do
    let s = balance bAccount bCommodity accounts
    unless (s == bAmount) (throwM $ BalanceDoesNotMatch bal s)
    return accounts
process accounts _ = pure accounts
