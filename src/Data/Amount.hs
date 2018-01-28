module Data.Amount
  ( Amount(..)
  ) where

import Data.Commodity (CommodityName)

data Amount a = Amount
  { _amount :: a
  , _commodity :: CommodityName
  } deriving (Eq, Show, Functor, Ord)
