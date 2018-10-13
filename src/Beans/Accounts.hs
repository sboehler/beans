module Beans.Accounts
  ( AccountsException(..)
  , Accounts
  , calculateAccountsForDays
  , checkLedger
  , check
  , process
  )
where

import qualified Data.List                     as L
import           Beans.Data.Accounts                      ( Account
                                                          , Accounts
                                                          , Amount
                                                          , Date
                                                          , Commodity
                                                          , Position(..)
                                                          )
import qualified Beans.Data.Accounts           as A
import           Beans.Data.Directives                    ( Command(..)
                                                          , Dated(..)
                                                          )
import qualified Beans.Data.Map                as M
import           Beans.Data.Restrictions                  ( Restriction
                                                          , Restrictions
                                                          )
import qualified Beans.Data.Restrictions       as R
import           Beans.Ledger                             ( Ledger )
import           Control.Monad                            ( unless
                                                          , foldM
                                                          , foldM_
                                                          , when
                                                          )
import           Control.Monad.Catch                      ( Exception
                                                          , MonadThrow
                                                          , throwM
                                                          )

data AccountsException
  = AccountIsNotOpen Command
  | BookingErrorAccountNotOpen Account
  | BookingErrorCommodityIncompatible Account
                                      Commodity
                                      Amount
                                      Restriction
  | AccountIsAlreadyOpen Command
  | BalanceIsNotZero Command
  | AccountDoesNotExist Command
  | BalanceDoesNotMatch Command
                        Amount
  deriving (Show)

instance Exception AccountsException

checkLedger :: (MonadThrow m) => Ledger -> m Ledger
checkLedger l = foldM_ check mempty (fmap undate l) >> pure l

calculateAccountsForDays
  :: (MonadThrow m) => Ledger -> [Date] -> Accounts -> m [Accounts]
calculateAccountsForDays ledger (day : days) initialAccounts = do
  let (previous, later) = L.span ((<= day) . date) ledger
  currAccounts <- foldM process initialAccounts (undate <$> previous)
  rest         <- calculateAccountsForDays later days currAccounts
  return $ currAccounts : rest
calculateAccountsForDays _ [] _ = return []

check :: (MonadThrow m) => Restrictions -> Command -> m Restrictions
check restrictions open@Open { oAccount, oRestriction } = do
  when (oAccount `M.member` restrictions) (throwM $ AccountIsAlreadyOpen open)
  return $ M.insert oAccount oRestriction restrictions
check restrictions closing@Close { cAccount } = do
  let (restriction, remainingRestrictions) =
        M.partitionWithKey (const . (== cAccount)) restrictions
  when (null restriction) (throwM $ AccountIsNotOpen closing)
  return remainingRestrictions
check restrictions Transaction { tPostings } = do
  mapM_ (g restrictions) $ M.toList tPostings
  return restrictions
 where
  g r (Position { pAccount, pCommodity }, s) = case M.lookup pAccount r of
    Nothing -> throwM $ BookingErrorAccountNotOpen pAccount
    Just r' -> unless
      (R.isCompatible r' pCommodity)
      (throwM $ BookingErrorCommodityIncompatible
        pAccount
        pCommodity
        (M.findWithDefaultM pCommodity s)
        r'
      )
check restrictions bal@Balance { bAccount } = do
  unless (bAccount `M.member` restrictions) $ throwM (AccountDoesNotExist bal)
  return restrictions
check restrictions _ = pure restrictions

process :: (MonadThrow m) => Accounts -> Command -> m Accounts
process accounts command@Close { cAccount } = do
  let (deletedAccount, remainingAccounts) =
        M.partitionKeys ((== cAccount) . pAccount) accounts
  unless ((all . all) (== 0) deletedAccount) (throwM $ BalanceIsNotZero command)
  return remainingAccounts
process accounts Transaction { tPostings } =
  return $ accounts `mappend` tPostings
process accounts bal@Balance { bAccount, bAmount, bCommodity } = do
  let s = A.balance bAccount bCommodity accounts
  unless (s == bAmount) (throwM $ BalanceDoesNotMatch bal s)
  return accounts
process accounts _ = pure accounts
