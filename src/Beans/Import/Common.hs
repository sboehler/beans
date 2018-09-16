module Beans.Import.Common where

import           Beans.Data.Accounts (Amount, CommodityName)
import           Data.Text           (Text)
import           Data.Time.Calendar  (Day)

data TransactionData = TransactionData
  { tdCurrency :: CommodityName
  , tdEntries  :: [Entry]
  } deriving (Eq, Show)

data Entry = Entry
  { eBookingDate :: Day
  , eDescription :: Text
  , eAmount      :: Amount
  , eValueDate   :: Day
  , eBalance     :: Maybe Amount
  } deriving (Eq, Show)
