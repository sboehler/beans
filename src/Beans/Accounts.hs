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
import           Control.Monad.State                      ( MonadState
                                                          , evalStateT
                                                          , get
                                                          , gets
                                                          , modify
                                                          , put
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
  evalStateT (mapM_ check commands >> get) restrictions


check :: (MonadThrow m, MonadState Restrictions m) => Command -> m ()
check (OpenCommand open@Open { oAccount, oRestriction }) = do
  restrictions <- get
  when (oAccount `M.member` restrictions) (throwM $ AccountIsAlreadyOpen open)
  modify $ M.insert oAccount oRestriction
check (CloseCommand closing@Close { cAccount }) = do
  (restriction, remainingRestrictions) <- gets
    (M.partitionWithKey (const . (== cAccount)))
  when (null restriction) (throwM $ AccountIsNotOpen closing)
  put remainingRestrictions
check (TransactionCommand Transaction { tPostings }) = do
  restrictions <- get
  mapM_ (g restrictions) $ M.toList tPostings
 where
  g restrictions ((a, c, _), s) = case M.lookup a restrictions of
    Nothing -> throwM $ BookingErrorAccountNotOpen a
    Just r  -> unless
      (R.isCompatible r c)
      (throwM $ BookingErrorCommodityIncompatible a c (M.findWithDefaultM c s) r
      )
check (BalanceCommand bal@Balance { bAccount }) = do
  r <- get
  unless (bAccount `M.member` r) (throwM $ AccountDoesNotExist bal)
check _ = pure ()

processTimestep :: (MonadThrow m) => Accounts -> Timestep -> m Accounts
processTimestep accounts (Timestep _ commands) =
  evalStateT (mapM_ process commands >> get) accounts

process :: (MonadThrow m, MonadState Accounts m) => Command -> m ()
process (CloseCommand closing@Close { cAccount }) = do
  (deletedAccount, remainingAccounts) <- gets (M.partitionWithKey g)
  unless ((all . all) (== 0) deletedAccount) (throwM $ BalanceIsNotZero closing)
  put remainingAccounts
  where g (a, _, _) _ = a == cAccount
process (TransactionCommand Transaction { tPostings }) =
  modify $ mappend tPostings
process (BalanceCommand bal@Balance { bAccount, bAmount, bCommodity }) = do
  s <- gets $ A.balance bAccount bCommodity
  unless (s == bAmount) (throwM $ BalanceDoesNotMatch bal s)
process _ = pure ()
