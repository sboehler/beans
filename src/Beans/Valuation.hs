module Beans.Valuation
  ( valuateLedger
  )
where


import           Beans.Accounts                 ( check
                                                , process
                                                )
import           Beans.Model             hiding ( filter )
import           Beans.Options                  ( Valuation(..) )
import qualified Beans.Data.Map                as M
import           Beans.Prices                   ( NormalizedPrices
                                                , Prices
                                                , lookupPrice
                                                , normalize
                                                , updatePrices
                                                )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.State            ( MonadState
                                                , evalStateT
                                                , StateT
                                                , get
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.Monoid                    ( Sum(Sum) )
import qualified Data.Text                     as T
import           Control.Lens


data ValuationState = ValuationState
  { _valuationStatePrices               :: Prices
  , _valuationStatePrevNormalizedPrices :: NormalizedPrices
  , _valuationStateNormalizedPrices     :: NormalizedPrices
  , _valuationStatePrevAccounts         :: Accounts
  , _valuationStateAccounts             :: Accounts
  , _valuationStateTarget               :: Commodity
  , _valuationStateValuationAccount     :: Account
  , _valuationStateRestrictions :: Restrictions
  }

makeFields ''ValuationState

valuateLedger :: MonadThrow m => Valuation -> Ledger -> m Ledger
valuateLedger (AtMarket t v) ledger = evalStateT
  (mapM valuate ledger)
  ValuationState { _valuationStatePrices               = mempty
                 , _valuationStatePrevNormalizedPrices = mempty
                 , _valuationStateNormalizedPrices     = mempty
                 , _valuationStatePrevAccounts         = mempty
                 , _valuationStateAccounts             = mempty
                 , _valuationStateTarget               = t
                 , _valuationStateValuationAccount     = v
                 , _valuationStateRestrictions         = mempty
                 }
valuateLedger _ ledger = pure ledger


valuate :: (MonadThrow m) => [Command] -> StateT ValuationState m [Command]
valuate commands = do
  prevNormalizedPrices <~ use normalizedPrices
  prevAccounts <~ use accounts

  zoom restrictions $ mapM_ check commands
  zoom accounts $ mapM process commands
  zoom prices $ mapM_ updatePrices commands >> get

  normalizedPrices <~ normalize <$> use prices <*> use target

  valuationTransactions <- adjustValuationForAccounts
  commands'             <- mapM processCommand $ filter notBalance commands
  return $ commands' ++ valuationTransactions

notBalance :: Command -> Bool
notBalance (CmdBalance _) = False
notBalance _              = True


processCommand
  :: (MonadThrow m, MonadState ValuationState m) => Command -> m Command
processCommand (CmdTransaction t) = do
  p <- mapM valuateAmounts $ t ^. postings
  v <- use valuationAccount
  CmdTransaction <$> mkBalancedTransaction (t ^. flag)
                                           (t ^. description)
                                           (t ^. tags)
                                           p
                                           (Just v)
processCommand c = pure c

valuateAmounts
  :: (MonadThrow m, MonadState ValuationState m) => Amounts -> m Amounts
valuateAmounts amounts = M.fromListM <$> mapM valuateAmount (M.toList amounts)

valuateAmount
  :: (MonadThrow m, MonadState ValuationState m)
  => (Commodity, Amount)
  -> m (Commodity, Amount)
valuateAmount (c, a) = do
  tc <- use target
  if tc == c
    then return (c, a)
    else do
      p <- use normalizedPrices >>= lookupPrice c
      return (tc, a * Sum p)

adjustValuationForAccounts
  :: (MonadThrow m, MonadState ValuationState m) => m [Command]
adjustValuationForAccounts = do
  a <- M.toList <$> use prevAccounts
  concat <$> sequence (adjustValuationForAccount <$> a)

adjustValuationForAccount
  :: (MonadThrow m, MonadState ValuationState m)
  => (Position, Amounts)
  -> m [Command]
adjustValuationForAccount (k, amounts) =
  catMaybes <$> mapM (adjustValuationForAmount k) (M.toList amounts)

adjustValuationForAmount
  :: (MonadThrow m, Monad m, MonadState ValuationState m)
  => Position
  -> (Commodity, Amount)
  -> m (Maybe Command)
adjustValuationForAmount k (c, a) = do
  v0 <- use prevNormalizedPrices >>= lookupPrice c
  v1 <- use normalizedPrices >>= lookupPrice c
  if v0 /= v1 && k ^. account . accountType `elem` [Assets, Liabilities]
    then Just <$> createValuationTransaction k (a * Sum (v1 - v0))
    else return Nothing

createValuationTransaction
  :: (Monad m, MonadState ValuationState m) => Position -> Amount -> m Command
createValuationTransaction p a = do
  t <- use target
  v <- use valuationAccount
  let desc = T.pack ("Valuation " <> p ^. (commodity . to show))
  return $ CmdTransaction $ Transaction Complete desc [] $ M.fromListM
    [(p, M.singleton t a), (p & account .~ v, M.singleton t (-a))]
