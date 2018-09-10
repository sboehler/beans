module Beans.Import.Common where

import           Beans.Data.Accounts (Amount, CommodityName)
import           Data.Text           (Text)
import           Data.Time.Calendar  (Day)

data TransactionData = TransactionData
  { _currency :: CommodityName
  , _entries  :: [Entry]
  } deriving (Eq, Show)

data Entry = Entry
  { _bookingDate :: Day
  , _description :: Text
  , _amount      :: Amount
  , _valueDate   :: Day
  , _balance     :: Maybe Amount
  } deriving (Eq, Show)
