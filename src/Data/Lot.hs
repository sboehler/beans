module Data.Lot
  ( Lot(..)
  ) where

import Data.Price (Price)
import Data.Text.Lazy (Text)
import Data.Time.Calendar (Day)

data Lot a = Lot
  { _cost :: Maybe (Price a)
  , _label :: Maybe Text
  , _date :: Maybe Day
  } deriving (Show, Eq)
