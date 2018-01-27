module Data.Posting
  ( PostingPrice(..)
  , Posting(..)
  ) where

import Data.AccountName (AccountName)
import Data.Amount (Amount)
import Data.Price (Price)
import Data.Text.Lazy (Text)
import Data.Time.Calendar (Day)

data Posting a = Posting
  { _postingAccountName :: AccountName
  , _amount :: Amount a
  , _price :: Maybe (PostingPrice a)
  , _lotCost :: Maybe (Price a)
  , _lotLabel :: Maybe Text
  , _lotDate :: Maybe Day
  } deriving (Show, Eq)

data PostingPrice a
  = UnitPrice { _unitPrice :: Price a }
  | TotalPrice { _totalPrice :: Amount a }
  deriving (Show, Eq)
