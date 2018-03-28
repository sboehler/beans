module Data.Amount
  ( Amount(..)
  ) where

import Data.Commodity (CommodityName)
import Data.Scientific(Scientific)

data Amount  = Amount
  { _amount :: Scientific
  , _commodity :: CommodityName
  } deriving (Eq, Show, Ord)
