module Beans.Accounts
  ( AccountsException(..)
  , Accounts
  , calculateAccounts
  , processTimestep
  , processTimestep'
  , checkTimestep
  , checkTimestep'
  )
where

import           Beans.Data.Accounts                      ( Account
                                                          , Accounts
                                                          , Amount
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
import           Data.Time.Calendar                       ( Day )

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

calculateAccounts :: (MonadThrow m) => Ledger -> m (M.Map Day Accounts)
calculateAccounts l = do
  evalStateT (mapM_ checkTimestep l)                  mempty
  evalStateT (M.fromListM <$> mapM processTimestep l) mempty

checkTimestep :: (MonadThrow m, MonadState Restrictions m) => Timestep -> m ()
checkTimestep (Timestep _ commands) = mapM_ check commands

checkTimestep'
  :: (MonadThrow m, MonadState Restrictions m) => Timestep -> m Restrictions
checkTimestep' (Timestep _ commands) = mapM_ check commands >> get


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

processTimestep
  :: (MonadThrow m, MonadState Accounts m) => Timestep -> m (Day, Accounts)
processTimestep (Timestep day commands) = do
  a <- mapM_ process commands >> get
  return (day, a)

processTimestep'
  :: (MonadThrow m, MonadState Accounts m) => Timestep -> m Accounts
processTimestep' (Timestep _ commands) = mapM_ process commands >> get


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
