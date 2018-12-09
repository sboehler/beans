module Beans.Valuation
  ( valuateLedger
  )
where


import           Beans.Accounts                 ( check
                                                , process
                                                )
import           Beans.Model                    ( Account(..)
                                                , Restrictions
                                                , AccountType(..)
                                                , Accounts
                                                , Amount
                                                , Amounts
                                                , Commodity(..)
                                                , Ledger
                                                , Position(..)
                                                , Command(..)
                                                , Transaction(..)
                                                , Flag(..)
                                                , mkBalancedTransaction
                                                )
import           Beans.Options                  ( Valuation(..) )
import qualified Beans.Data.Map                as M
import           Beans.Prices                   ( NormalizedPrices
                                                , Prices
                                                , lookupPrice
                                                , normalize
                                                , updatePrices
                                                )
import           Control.Monad                  ( foldM )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.State            ( MonadState
                                                , evalStateT
                                                , get
                                                , gets
                                                , put
                                                )
import           Data.Monoid                    ( Sum(Sum) )
import qualified Data.List                     as L
import qualified Data.Text                     as T


data ValuationState = ValuationState
  { vsPrices               :: Prices
  , vsPrevNormalizedPrices :: NormalizedPrices
  , vsNormalizedPrices     :: NormalizedPrices
  , vsPrevAccounts         :: Accounts
  , vsAccounts             :: Accounts
  , vsTarget               :: Commodity
  , vsValuationAccount     :: Account
  , vsRestrictions :: Restrictions
  }

valuateLedger :: MonadThrow m => Valuation -> Ledger -> m Ledger
valuateLedger (AtMarket target valuationAccount) ledger = evalStateT
  (mapM valuate ledger)
  ValuationState
    { vsPrices               = mempty
    , vsPrevNormalizedPrices = mempty
    , vsNormalizedPrices     = mempty
    , vsPrevAccounts         = mempty
    , vsAccounts             = mempty
    , vsTarget               = target
    , vsValuationAccount     = valuationAccount
    , vsRestrictions         = mempty
    }
valuateLedger _ ledger = pure ledger


valuate
  :: (MonadThrow m, MonadState ValuationState m) => [Command] -> m [Command]
valuate commands = do
  v@ValuationState {..} <- get
  let vsPrices' = L.foldl' updatePrices vsPrices commands
  vsRestrictions' <- foldM check vsRestrictions commands
  vsAccounts'     <- foldM process vsAccounts commands
  put $ v { vsPrices               = vsPrices'
          , vsAccounts             = vsAccounts'
          , vsRestrictions         = vsRestrictions'
          , vsPrevNormalizedPrices = vsNormalizedPrices
          , vsNormalizedPrices     = normalize vsPrices' vsTarget
          , vsPrevAccounts         = vsAccounts
          }
  valuationTransactions <- adjustValuationForAccounts
  commands'             <- mapM processCommand $ filter notBalance commands
  return $ commands' ++ valuationTransactions

notBalance :: Command -> Bool
notBalance (CmdBalance _) = False
notBalance _              = True


processCommand
  :: (MonadThrow m, MonadState ValuationState m) => Command -> m Command
processCommand (CmdTransaction Transaction {..}) = do
  ValuationState { vsValuationAccount } <- get
  postings <- mapM valuateAmounts _transactionPostings
  CmdTransaction <$> mkBalancedTransaction _transactionFlag
                                           _transactionDescription
                                           _transactionTags
                                           postings
                                           (Just vsValuationAccount)
processCommand c = pure c

valuateAmounts
  :: (MonadThrow m, MonadState ValuationState m) => Amounts -> m Amounts
valuateAmounts amounts = M.fromListM <$> mapM valuateAmount (M.toList amounts)

valuateAmount
  :: (MonadThrow m, MonadState ValuationState m)
  => (Commodity, Amount)
  -> m (Commodity, Amount)
valuateAmount a@(commodity, amount) = do
  tc <- gets vsTarget
  if tc == commodity
    then return a
    else do
      price <- gets vsNormalizedPrices >>= lookupPrice commodity
      return (tc, amount * Sum price)

adjustValuationForAccounts
  :: (MonadThrow m, MonadState ValuationState m) => m [Command]
adjustValuationForAccounts = do
  a <- M.toList <$> gets vsPrevAccounts
  concat <$> sequence (adjustValuationForAccount <$> a)

adjustValuationForAccount
  :: (MonadThrow m, MonadState ValuationState m)
  => (Position, Amounts)
  -> m [Command]
adjustValuationForAccount (k, amounts) =
  concat <$> mapM (adjustValuationForAmount k) (M.toList amounts)

adjustValuationForAmount
  :: (MonadThrow m, MonadState ValuationState m)
  => Position
  -> (Commodity, Amount)
  -> m [Command]
adjustValuationForAmount k (commodity, amount) = do
  v0 <- gets vsPrevNormalizedPrices >>= lookupPrice commodity
  v1 <- gets vsNormalizedPrices >>= lookupPrice commodity
  if v0
       /=     v1
       &&     (_accountAccountType . _positionAccount) k
       `elem` [Assets, Liabilities]
    then pure <$> createValuationTransaction k (amount * Sum (v1 - v0))
    else return []

createValuationTransaction
  :: (MonadState ValuationState m) => Position -> Amount -> m Command
createValuationTransaction position amount = do
  ValuationState { vsTarget, vsValuationAccount } <- get
  let desc = T.pack ("Valuation " <> show (_positionCommodity position))
  return $ CmdTransaction $ Transaction Complete desc [] $ M.fromListM
    [ (position, M.singleton vsTarget amount)
    , ( position { _positionAccount = vsValuationAccount }
      , M.singleton vsTarget (-amount)
      )
    ]
