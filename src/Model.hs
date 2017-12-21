module Model
  ( AccountName(..)
  , CommodityName(..)
  , PostingAmount(..)
  , ConfigDirective(..)
  , Posting(..)
  , Directive(..)
  , DatedDirective(..)
  , PostingPrice(..)
  , Tag(..)
  , Cost(..)
  , Flag(..)
  ) where

import Data.Decimal (Decimal)
import Data.Text.Lazy (Text)
import Data.Time.Calendar (Day)

-- Type to wrap the AST of a file
data DatedDirective
  = AccountOpen { _accountName :: AccountName
                , _commodities :: [CommodityName] }
  | AccountClose { _accountName :: AccountName }
  | Balance { _accountName :: AccountName
            , _amount :: Decimal
            , _commodity :: CommodityName }
  | Transaction { _flag :: Flag
                , _description :: Text
                , _tags :: [Tag]
                , _postings :: [Posting] }
  | Price { _commodity :: CommodityName
          , _price :: Decimal
          , _priceCommodity :: CommodityName }
  deriving (Eq, Show)

data ConfigDirective
  = Include FilePath
  | Option Text
           Text
  deriving (Show, Eq)

-- Type to wrap the AST of a file
data Directive
  = Config ConfigDirective
  | Dated { _date :: Day
          , _directive :: DatedDirective }
  deriving (Show, Eq)

data Flag
  = Complete
  | Incomplete
  deriving (Eq, Show)

newtype Tag =
  Tag Text
  deriving (Show, Eq)

data Posting = Posting
  { _accountName :: AccountName
  , _amount :: Maybe PostingAmount
  } deriving (Show, Eq)

newtype AccountName =
  AccountName [Text]
  deriving (Show, Eq)

data PostingAmount = PostingAmount
  { _amount :: Decimal
  , _commodity :: CommodityName
  , _cost :: Maybe Cost
  , _price :: Maybe PostingPrice
  } deriving (Show, Eq)

newtype CommodityName =
  CommodityName Text
  deriving (Show, Eq)

data Cost = Cost
  { _amount :: Decimal
  , _commodity :: CommodityName
  , _label :: Maybe Day
  } deriving (Show, Eq)

data PostingPrice
  = UnitPrice { _amount :: Decimal
              , _commodity :: CommodityName }
  | TotalPrice { _amount :: Decimal
               , _commodity :: CommodityName }
  deriving (Show, Eq)
