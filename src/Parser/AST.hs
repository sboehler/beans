module Parser.AST where

import           Data.Scientific    (Scientific)
import           Data.Text.Lazy     (Text, intercalate, unpack)
import           Data.Time.Calendar (Day)

data Directive a
  = Opn Open
        a
  | Bal Balance
        a
  | Trn Transaction
        a
  | Cls Close
        a
  | Prc Price
        a
  | Opt Option
        a
  | Inc Include
        a
  deriving (Eq, Show, Functor, Ord)

data Balance = Balance
  { _date      :: Day
  , _account   :: AccountName
  , _amount    :: Scientific
  , _commodity :: CommodityName
  } deriving (Eq, Show, Ord)

data Open = Open
  { _date        :: Day
  , _accountName :: AccountName
  , _commodities :: [CommodityName]
  } deriving (Show, Eq, Ord)

data Close = Close
  { _date    :: Day
  , _account :: AccountName
  } deriving (Show, Eq, Ord)

data Price = Price
  { _date            :: Day
  , _commodity       :: CommodityName
  , _price           :: Scientific
  , _targetCommodity :: CommodityName
  } deriving (Show, Eq, Ord)

data Transaction = Transaction
  { _date        :: Day
  , _flag        :: Flag
  , _description :: Text
  , _tags        :: [Tag]
  , _postings    :: [Posting]
  } deriving (Eq, Show, Ord)

data Posting
  = Posting { _account   :: AccountName
            , _amount    :: Scientific
            , _commodity :: CommodityName
            , _lot       :: Maybe Lot }
  | Wildcard AccountName
  deriving (Show, Eq, Ord)

data Flag
  = Complete
  | Incomplete
  deriving (Eq, Show, Ord)

newtype Tag =
  Tag Text
  deriving (Show, Eq, Ord)

data Lot = Lot
  { _price           :: Scientific
  , _targetCommodity :: CommodityName
  , _date            :: Day
  , _label           :: Maybe Text
  } deriving (Show, Eq, Ord)

newtype Include = Include
  { _filePath :: FilePath
  } deriving (Show, Eq, Ord)

data Option =
  Option Text
         Text
  deriving (Show, Eq, Ord)

newtype AccountName = AccountName
  { _unAccountName :: [Text]
  } deriving (Eq, Ord)

instance Show AccountName where
  show = unpack . intercalate ":" . _unAccountName

newtype CommodityName = CommodityName
  { _unCommodityName :: Text
  } deriving (Show, Eq, Ord)
