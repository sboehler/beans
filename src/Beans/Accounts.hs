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
import           Control.Lens

data AccountsException
  = AccountIsNotOpen Close
  | BookingErrorAccountNotOpen Position
  | BookingErrorCommodityIncompatible Position
  | AccountIsAlreadyOpen Open
  | BalanceIsNotZero Close
  | AccountDoesNotExist Balance
  | BalanceDoesNotMatch Balance
                        Amount
  deriving (Show)

instance Exception AccountsException

sumUntil :: (MonadThrow m) => Date -> Ledger -> Accounts -> m (Accounts, Ledger)
sumUntil d ledger accounts = do
  let (previous, later) = M.partitionWithKey (const . (<= d)) ledger
      commands          = mconcat $ M.elems previous
  accounts' <- evalStateT (mapM_ process commands >> get) accounts
  return (accounts', later)

check :: (MonadState Restrictions m, MonadThrow m) => Command -> m ()
check (CmdOpen open) = do
  alreadyOpen <- gets $ M.member (open ^. account)
  when alreadyOpen $ throwM $ AccountIsAlreadyOpen open
  modify $ M.insert (open ^. account) (open ^. restriction)
check (CmdClose closing) = do
  (r, remainingRestrictions) <- gets
    $ M.partitionWithKey (const . (== closing ^. account))
  when (null r) (throwM $ AccountIsNotOpen closing)
  put remainingRestrictions
check (CmdTransaction t) = mapM_ checkPosition (t ^. postings . to M.keys)
 where
  checkPosition position = do
    r <- gets $ M.lookup (position ^. account)
    case r of
      Nothing -> throwM $ BookingErrorAccountNotOpen position
      Just r' -> unless (isCompatible r' (position ^. commodity))
                        (throwM $ BookingErrorCommodityIncompatible position)
check (CmdBalance bal) = do
  isOpen <- gets $ M.member (bal ^. account)
  unless isOpen $ throwM (AccountDoesNotExist bal)
check _ = pure ()

process :: (MonadState Accounts m, MonadThrow m) => Command -> m ()
process (CmdClose close) = do
  (del, others) <- gets
    $ M.partitionKeys (\pos -> pos ^. account == close ^. account)
  unless ((all . all) (== 0) del) (throwM $ BalanceIsNotZero close)
  put others
process (CmdTransaction t  ) = modify (mappend $ t ^. postings)
process (CmdBalance     bal) = do
  s <- gets $ balance (bal ^. account) (bal ^. commodity)
  unless (s == bal ^. amount) (throwM $ BalanceDoesNotMatch bal s)
process _ = pure ()
