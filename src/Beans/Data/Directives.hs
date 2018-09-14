module Beans.Data.Directives where

import           Beans.Data.Accounts     (AccountName, Amount, CommodityName,
                                          Lot (..))
import qualified Beans.Data.Map          as M
import           Beans.Data.Restrictions (Restriction)
import           Control.Monad.Catch     (Exception, MonadThrow, throwM)
import           Data.Monoid             (Sum (Sum))
import           Data.Scientific         (Scientific)
import           Data.Text               (Text)
import           Data.Time.Calendar      (Day)
import qualified Text.Megaparsec.Pos     as P

data Directive
  = Bal Balance
  | Opn Open
  | Cls Close
  | Trn Transaction
  | Prc Price
  | Opt Option
  | Inc Include
  deriving (Show)

data Balance = Balance
  { _pos       :: Maybe P.SourcePos
  , _date      :: Day
  , _account   :: AccountName
  , _amount    :: Amount
  , _commodity :: CommodityName
  } deriving (Show)

data Open = Open
  { _pos         :: Maybe P.SourcePos
  , _date        :: Day
  , _account     :: AccountName
  , _restriction :: Restriction
  } deriving (Show)

data Close = Close
  { _pos     :: Maybe P.SourcePos
  , _date    :: Day
  , _account :: AccountName
  } deriving (Show)

data Price = Price
  { _pos             :: P.SourcePos
  , _date            :: Day
  , _commodity       :: CommodityName
  , _price           :: Scientific
  , _targetCommodity :: CommodityName
  } deriving (Show)

data Transaction = Transaction
  { _pos         :: Maybe P.SourcePos
  , _date        :: Day
  , _flag        :: Flag
  , _description :: Text
  , _tags        :: [Tag]
  , _postings    :: [Posting]
  } deriving (Show)

data Posting = Posting
  { _pos       :: Maybe P.SourcePos
  , _account   :: AccountName
  , _amount    :: Amount
  , _commodity :: CommodityName
  , _lot       :: Maybe Lot
  } deriving (Show)

data Flag
  = Complete
  | Incomplete
  deriving (Show)

newtype Tag =
  Tag Text
  deriving (Show)

data Include = Include
  { _pos      :: P.SourcePos
  , _filePath :: FilePath
  } deriving (Show)

data Option =
  Option P.SourcePos
         Text
         Text
  deriving (Show)

data UnbalancedTransaction =
  UnbalancedTransaction
  deriving (Eq, Show)

instance Exception UnbalancedTransaction

mkBalancedTransaction :: MonadThrow m =>
     Maybe P.SourcePos
  -> Day
  -> Flag
  -> Text
  -> [Tag]
  -> [Posting]
  -> Maybe AccountName
  -> m Transaction
mkBalancedTransaction pos day flag desc tags ps wildcard =
  Transaction pos day flag desc tags <$> completePostings ps wildcard

completePostings :: MonadThrow m =>
     [Posting] -> Maybe AccountName -> m [Posting]
completePostings postings wildcard = do
  let imbalances = calculateImbalances postings
  fixes <- fixImbalances wildcard imbalances
  return $ postings ++ fixes
   where
    fixImbalances _ []       = return []
    fixImbalances (Just a) i = return $ balanceImbalance a <$> i
    fixImbalances _ _        = throwM UnbalancedTransaction

balanceImbalance :: AccountName -> (CommodityName, Amount) -> Posting
balanceImbalance _account (c, a) =
  Posting
    {_pos = Nothing, _amount = negate a, _commodity = c, _lot = Nothing, ..}

calculateImbalances :: [Posting] -> [(CommodityName, Amount)]
calculateImbalances =
  M.toList . M.filter ((> Sum 0.005) . abs) . M.fromList . fmap weight

weight :: Posting -> (CommodityName, Amount)
weight Posting {..} =
  case _lot of
    Just Lot {_price, _targetCommodity} -> (_targetCommodity, _amount * _price)
    Nothing -> (_commodity, _amount)
