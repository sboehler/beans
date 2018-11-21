module Beans.Accounts
  ( Accounts
  , calculateAccountsForDays
  , check
  , process
  )
where

import           Beans.Model                              ( Account
                                                          , Accounts
                                                          , Amount
                                                          , Date
                                                          , Ledger
                                                          , Restriction
                                                          , Restrictions
                                                          , Commodity
                                                          , Position(..)
                                                          , Command(..)
                                                          , Dated(..)
                                                          , balance
                                                          , isCompatible
                                                          )
import qualified Data.List                     as L
import qualified Beans.Data.Map                as M
import           Control.Monad                            ( unless
                                                          , foldM
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
      (isCompatible r' pCommodity)
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
  let s = balance bAccount bCommodity accounts
  unless (s == bAmount) (throwM $ BalanceDoesNotMatch bal s)
  return accounts
process accounts _ = pure accounts
