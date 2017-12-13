module Model where

import Data.Decimal (Decimal)
import Data.Text (Text)
import Data.Time.Calendar (Day)

data Transaction = T
  { date :: Day
  , flag :: Char
  , description :: Text
  , postings :: [Posting]
  } deriving (Show)

data Posting
  = Posting { accountName :: AccountName
            , amount :: Decimal
            , commodity :: Commodity }
  | WildcardPosting { accountName :: AccountName }
  deriving (Show)

newtype Commodity =
  Commodity Text
  deriving (Show)

newtype AccountName =
  AccountName [Text]
  deriving (Show)

data Account = Account
  { accountName :: AccountName
  , commodities :: [Commodity]
  } deriving (Show)

data AccountOpen = AccountOpen
  { date :: Day
  , accountName :: AccountName
  , commodities :: [Commodity]
  } deriving (Show)

data AccountClose = AccountClose
  { date :: Day
  , accountName :: AccountName
  } deriving (Show)

data Balance = Balance
  { date :: Day
  , accountName :: AccountName
  , amount :: Decimal
  , commodity :: Commodity
  } deriving (Show)

data Price = Price
  { date :: Day
  , commodity :: Commodity
  , price :: Decimal
  , priceCommodity :: Commodity
  } deriving (Show)

data Directive
  = DTransaction Transaction
  | DAccountOpen AccountOpen
  | DAccountClose AccountClose
  | DPrice Price
  | DBalance Balance
  deriving (Show)
