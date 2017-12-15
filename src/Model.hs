module Model where

import Data.Decimal (Decimal)
import Data.Text (Text)
import Data.Time.Calendar (Day)

newtype CommodityName =
  CommodityName Text
  deriving (Show)

data Amount = Amount
  { _amount :: Decimal
  , _commodity :: CommodityName
  } deriving (Show)

data Posting = Posting
  { _accountName :: AccountName
  , _amount :: Maybe Amount
  } deriving (Show)

newtype AccountName =
  AccountName [Text]
  deriving (Show)

data Directive
  = Transaction { _date :: Day
                , _flag :: Char
                , _description :: Text
                , _postings :: [Posting] }
  | AccountOpen { _date :: Day
                , _accountName :: AccountName
                , _commodities :: [CommodityName] }
  | AccountClose { _date :: Day
                 , _accountName :: AccountName }
  | Balance { _date :: Day
            , _accountName :: AccountName
            , _amount :: Decimal
            , _commodity :: CommodityName }
  | Price { _date :: Day
          , _commodity :: CommodityName
          , _price :: Decimal
          , _priceCommodity :: CommodityName }
  | Include FilePath
  | Option Text
           Text
  deriving (Show)
