module Data.Lot
  ( Lot(..)
  ) where

import Data.Amount (Amount)
import Data.Text.Lazy (Text)
import Data.Time.Calendar (Day)

data Lot = Lot
  { _cost :: Amount 
  , _date :: Day
  , _label :: Maybe Text
  } deriving (Show, Eq, Ord)
