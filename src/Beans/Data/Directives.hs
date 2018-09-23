module Beans.Data.Directives
  ( Command(..)
  , DatedCommand(..)
  , Directive(..)
  , Transaction(..)
  , mkBalancedTransaction
  , Balance(..)
  , Posting
  , Open(..)
  , Close(..)
  , Include(..)
  , Option(..)
  , Price(..)
  , Tag(..)
  , Flag(..)
  ) where

import           Beans.Data.Accounts     (AccountName, Accounts, Amount,
                                          Amounts, CommodityName, Lot (..))
import qualified Beans.Data.Map          as M
import           Beans.Data.Restrictions (Restriction)
import           Control.Monad.Catch     (Exception, MonadThrow, throwM)
import           Data.Scientific         (Scientific)
import           Data.Text               (Text)
import           Data.Time.Calendar      (Day)
import qualified Text.Megaparsec.Pos     as P

data Directive
  = DatedCommandDirective DatedCommand
  | OptionDirective Option
  | IncludeDirective Include
  deriving (Eq, Ord, Show)

data DatedCommand =
  DatedCommand Day
               Command
  deriving (Eq, Ord, Show)

data Command
  = BalanceCommand Balance
  | OpenCommand Open
  | CloseCommand Close
  | TransactionCommand Transaction
  | PriceCommand Price
  deriving (Eq, Ord, Show)

data Balance = Balance
  { bAccount   :: AccountName
  , bAmount    :: Amount
  , bCommodity :: CommodityName
  } deriving (Eq, Ord, Show)

data Open = Open
  { oAccount     :: AccountName
  , oRestriction :: Restriction
  } deriving (Eq, Ord, Show)

newtype Close = Close
  { cAccount :: AccountName
  } deriving (Eq, Ord, Show)

data Price = Price
  { pCommodity       :: CommodityName
  , pPrice           :: Scientific
  , pTargetCommodity :: CommodityName
  } deriving (Eq, Ord, Show)

data Transaction = Transaction
  { tFlag        :: Flag
  , tDescription :: Text
  , tTags        :: [Tag]
  , tPostings    :: Accounts
  } deriving (Eq, Show)

instance Ord Transaction where
  _ `compare` _ = EQ

data Flag
  = Complete
  | Incomplete
  deriving (Eq, Ord, Show)

newtype Tag =
  Tag Text
  deriving (Eq, Ord, Show)

data Include = Include
  { iPos      :: P.SourcePos
  , iFilePath :: FilePath
  } deriving (Eq, Ord, Show)

data Option =
  Option P.SourcePos
         Text
         Text
  deriving (Eq, Ord, Show)

data UnbalancedTransaction =
  UnbalancedTransaction Accounts
                        (M.Map CommodityName Amount)
  deriving (Eq, Show)

type Posting = ((AccountName, CommodityName, Maybe Lot), Amounts)

instance Exception UnbalancedTransaction

mkBalancedTransaction ::
     MonadThrow m
  => Flag
  -> Text
  -> [Tag]
  -> Accounts
  -> Maybe AccountName
  -> m Transaction
mkBalancedTransaction flag desc tags postings wildcard =
  Transaction flag desc tags <$> completePostings wildcard postings

completePostings :: MonadThrow m => Maybe AccountName -> Accounts -> m Accounts
completePostings wildcard postings =
  let imbalances = calculateImbalances postings
   in mappend postings <$> fixImbalances wildcard imbalances
  where
    fixImbalances w i
      | null i = return mempty
      | M.size i == 1 = case w of
          Just account -> return $ balanceImbalances account i
          Nothing      -> throwM $ UnbalancedTransaction postings i
      | otherwise      = return mempty

calculateImbalances :: Accounts -> M.Map CommodityName Amount
calculateImbalances = mconcat . fmap snd . M.toList

balanceImbalances :: AccountName -> M.Map CommodityName Amount -> Accounts
balanceImbalances account = M.mapEntries g . fmap negate
  where
    g (c, s) = ((account, c, Nothing), M.singleton c s)
