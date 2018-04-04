module Haricot.AST where

import           Data.Scientific     (Scientific)
import           Data.Text.Lazy      (Text, intercalate, unpack)
import           Data.Time.Calendar  (Day)
import qualified Text.Megaparsec.Pos as P

data Directive
  = Bal Balance
  | Opn Open
  | Cls Close
  | Trn Transaction
  | Prc Price
  | Opt Option
  | Inc Include
  deriving (Eq, Show, Ord)

data Balance = Balance
  { _pos       :: P.SourcePos
  , _date      :: Day
  , _account   :: AccountName
  , _amount    :: Scientific
  , _commodity :: CommodityName
  } deriving (Eq, Show, Ord)

data Open = Open
  { _pos         :: P.SourcePos
  , _date        :: Day
  , _account     :: AccountName
  , _commodities :: [CommodityName]
  } deriving (Show, Eq, Ord)

data Close = Close
  { _pos     :: P.SourcePos
  , _date    :: Day
  , _account :: AccountName
  } deriving (Show, Eq, Ord)

data Price = Price
  { _pos             :: P.SourcePos
  , _date            :: Day
  , _commodity       :: CommodityName
  , _price           :: Scientific
  , _targetCommodity :: CommodityName
  } deriving (Show, Eq, Ord)

data Transaction = Transaction
  { _pos         :: P.SourcePos
  , _date        :: Day
  , _flag        :: Flag
  , _description :: Text
  , _tags        :: [Tag]
  , _postings    :: [Posting]
  } deriving (Eq, Show, Ord)

data Posting
  = Posting { _pos       :: P.SourcePos
            , _account   :: AccountName
            , _amount    :: Scientific
            , _commodity :: CommodityName
            , _lot       :: Maybe Lot }
  | Wildcard { _pos     :: P.SourcePos
             , _account :: AccountName }
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

data Include = Include
  {
    _pos      :: P.SourcePos,
    _filePath :: FilePath
  } deriving (Show, Eq, Ord)

data Option =
  Option P.SourcePos Text
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
