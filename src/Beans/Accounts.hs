module Beans.Accounts
  ( AccountsException(..)
  , Accounts
  , calculateAccountsForDays
  , checkLedger
  , processTimestep
  , checkTimestep
  )
where

import qualified Data.List                     as L
import           Beans.Data.Accounts                      ( Account
                                                          , Accounts
                                                          , Amount
                                                          , Date
                                                          , Commodity
                                                          )
import qualified Beans.Data.Accounts           as A
import           Beans.Data.Directives                    ( Balance(..)
                                                          , Close(..)
                                                          , Command(..)
                                                          , Open(..)
                                                          , Transaction(..)
                                                          )
import qualified Beans.Data.Map                as M
import           Beans.Data.Restrictions                  ( Restriction
                                                          , Restrictions
                                                          )
import qualified Beans.Data.Restrictions       as R
import           Beans.Ledger                             ( Ledger
                                                          , Timestep(..)
                                                          )
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

checkLedger :: (MonadThrow m) => Ledger -> m Ledger
checkLedger l = foldM_ checkTimestep mempty l >> pure l

calculateAccountsForDays
  :: (MonadThrow m) => Ledger -> [Date] -> Accounts -> m [Accounts]
calculateAccountsForDays ledger (day : days) initialAccounts = do
  let (previous, later) = L.span ((<= day) . tsDate) ledger
  currAccounts <- foldM processTimestep initialAccounts previous
  rest         <- calculateAccountsForDays later days currAccounts
  return $ currAccounts : rest
calculateAccountsForDays _ [] _ = return []

checkTimestep :: (MonadThrow m) => Restrictions -> Timestep -> m Restrictions
checkTimestep restrictions (Timestep _ commands) =
  foldM check restrictions commands

check :: (MonadThrow m) => Restrictions -> Command -> m Restrictions
check restrictions (OpenCommand open@Open { oAccount, oRestriction }) = do
  when (oAccount `M.member` restrictions) (throwM $ AccountIsAlreadyOpen open)
  return $ M.insert oAccount oRestriction restrictions
check restrictions (CloseCommand closing@Close { cAccount }) = do
  let (restriction, remainingRestrictions) =
        M.partitionWithKey (const . (== cAccount)) restrictions
  when (null restriction) (throwM $ AccountIsNotOpen closing)
  return remainingRestrictions
check restrictions (TransactionCommand Transaction { tPostings }) = do
  mapM_ (g restrictions) $ M.toList tPostings
  return restrictions
 where
  g r ((a, c, _), s) = case M.lookup a r of
    Nothing -> throwM $ BookingErrorAccountNotOpen a
    Just r' -> unless
      (R.isCompatible r' c)
      ( throwM
      $ BookingErrorCommodityIncompatible a c (M.findWithDefaultM c s) r'
      )
check restrictions (BalanceCommand bal@Balance { bAccount }) = do
  unless (bAccount `M.member` restrictions) $ throwM (AccountDoesNotExist bal)
  return restrictions
check restrictions _ = pure restrictions

processTimestep :: (MonadThrow m) => Accounts -> Timestep -> m Accounts
processTimestep accounts (Timestep _ commands) =
  foldM process accounts commands

process :: (MonadThrow m) => Accounts -> Command -> m Accounts
process accounts (CloseCommand closing@Close { cAccount }) = do
  let (deletedAccount, remainingAccounts) = M.partitionWithKey g accounts
  unless ((all . all) (== 0) deletedAccount) (throwM $ BalanceIsNotZero closing)
  return remainingAccounts
  where g (a, _, _) _ = a == cAccount
process accounts (TransactionCommand Transaction { tPostings }) =
  return $ accounts `mappend` tPostings
process accounts (BalanceCommand bal@Balance { bAccount, bAmount, bCommodity })
  = do
    let s = A.balance bAccount bCommodity accounts
    unless (s == bAmount) (throwM $ BalanceDoesNotMatch bal s)
    return accounts
process accounts _ = pure accounts
