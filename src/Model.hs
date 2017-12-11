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
  | WildcardPosting { account :: Account }
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
