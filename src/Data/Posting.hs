module Data.Posting
  ( PostingPrice(..)
  , Posting(..)
  ) where

import Data.AccountName (AccountName)
import Data.Amount (Amount)
import Data.Commodity (CommodityName)
import Data.Price (Price)
import Data.Text.Lazy (Text)
import Data.Time.Calendar (Day)

data Posting a = Posting
  { _accountName :: AccountName
  , _amount :: a
  , _commodity :: CommodityName
  , _price :: Maybe (PostingPrice a)
  , _lotCost :: Maybe (Price a)
  , _lotLabel :: Maybe Text
  , _lotDate :: Maybe Day
  } deriving (Show, Eq)

data PostingPrice a
  = UnitPrice (Price a)
  | TotalPrice (Amount a)
  deriving (Show, Eq)
