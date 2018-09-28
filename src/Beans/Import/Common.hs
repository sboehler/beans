module Beans.Import.Common where

import           Beans.Data.Accounts (Amount, CommodityName)
import           Control.Exception   (Exception)
import           Data.Text           (Text)
import           Data.Time.Calendar  (Day)

-- The exception exported by this module
newtype ImporterException =
  ImporterException String
  deriving (Eq)

instance Show ImporterException where
  show (ImporterException s) = s

instance Exception ImporterException

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
