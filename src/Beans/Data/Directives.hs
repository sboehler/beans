module Beans.Data.Directives
  ( Command(..)
  , DatedCommand(..)
  , Directive(..)
  , Posting(..)
  , Transaction(..)
  , mkBalancedTransaction
  , Balance(..)
  , Open(..)
  , Close(..)
  , Include(..)
  , Option(..)
  , Price(..)
  , Tag(..)
  , Flag(..)
  ) where

import           Beans.Data.Accounts     (AccountName, Amount, CommodityName,
                                          Lot (..), Posting (..))
import qualified Beans.Data.Map          as M
import           Beans.Data.Restrictions (Restriction)
import           Control.Monad.Catch     (Exception, MonadThrow, throwM)
import           Data.Monoid             (Sum (Sum))
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
  , tPostings    :: [Posting]
  } deriving (Eq, Ord, Show)

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
  UnbalancedTransaction
  deriving (Eq, Show)

instance Exception UnbalancedTransaction

mkBalancedTransaction ::
     MonadThrow m
  => Flag
  -> Text
  -> [Tag]
  -> [Posting]
  -> Maybe AccountName
  -> m Transaction
mkBalancedTransaction flag desc tags ps wildcard =
  Transaction flag desc tags <$> completePostings ps wildcard

completePostings ::
     MonadThrow m => [Posting] -> Maybe AccountName -> m [Posting]
completePostings postings wildcard = do
  let imbalances = calculateImbalances postings
  fixes <- fixImbalances wildcard imbalances
  return $ postings ++ fixes
  where
    fixImbalances _ []       = return []
    fixImbalances (Just a) i = return $ balanceImbalance a <$> i
    fixImbalances _ _        = throwM UnbalancedTransaction

balanceImbalance :: AccountName -> (CommodityName, Amount) -> Posting
balanceImbalance pAccount (c, a) =
  Posting {pAmount = negate a, pCommodity = c, pLot = Nothing, pAccount}

calculateImbalances :: [Posting] -> [(CommodityName, Amount)]
calculateImbalances =
  M.toList . M.filter ((> Sum 0.005) . abs) . M.fromList . fmap weight

weight :: Posting -> (CommodityName, Amount)
weight Posting {..} =
  case pLot of
    Just Lot {lPrice, lTargetCommodity} -> (lTargetCommodity, pAmount * lPrice)
    Nothing -> (pCommodity, pAmount)
