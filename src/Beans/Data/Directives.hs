module Beans.Data.Directives
   where

import           Beans.Data.Accounts     (AccountName, Amount, CommodityName,
                                          Lot)
import           Beans.Data.Restrictions (Restriction)
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
  {
    _pos      :: P.SourcePos,
    _filePath :: FilePath
  } deriving (Show)

data Option =
  Option P.SourcePos
         Text
         Text
  deriving (Show)
