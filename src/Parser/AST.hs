module Parser.AST where

import Control.Exception (Exception)
import Data.Account (AccountName(..))
import Data.Amount (Amount)
import Data.Commodity (CommodityName(..))
import Data.Decimal (Decimal)
import Data.Posting (Posting)
import Data.Text.Lazy (Text)
import Data.Time.Calendar (Day)
import Data.Transaction (Transaction)
import Text.Parsec (ParseError)

newtype ParseException =
  ParseException ParseError
  deriving (Show)

instance Exception ParseException

-- Type to wrap the AST of a file
data Directive a
  = Opn Open
        a
  | Cls Close
        a
  | Bal Balance
        a
  | Trn Transaction
        a
  | Prc PriceDirective
        a
  | Opt Option
        a
  | Inc Include
        a
  deriving (Eq, Show, Functor)

data Balance = Balance
  { _date :: Day
  , _accountName :: AccountName
  , _amount :: Amount Decimal
  } deriving (Eq, Show)

data Open = Open
  { _date :: Day
  , _accountName :: AccountName
  , _commodities :: [CommodityName]
  } deriving (Show, Eq)

data Close = Close
  { _date :: Day
  , _accountName :: AccountName
  } deriving (Show, Eq)

data PriceDirective = PriceDirective
  { _date :: Day
  , _commodity :: CommodityName
  , _price :: Amount Decimal
  } deriving (Show, Eq)

newtype Include = Include
  { _filePath :: FilePath
  } deriving (Show, Eq)

data Option =
  Option Text
         Text
  deriving (Show, Eq)

data PostingDirective
  = WildcardPosting AccountName
  | CompletePosting (Posting Decimal)
  deriving (Show, Eq)
