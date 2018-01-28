module Data.Lot
  ( Lot(..)
  ) where

import Data.Amount (Amount)
import Data.Text.Lazy (Text)
import Data.Time.Calendar (Day)

data Lot a = Lot
  { _cost :: Maybe (Amount a)
  , _label :: Maybe Text
  , _date :: Maybe Day
  } deriving (Show, Eq, Ord)
