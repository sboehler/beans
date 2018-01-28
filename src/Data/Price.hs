module Data.Price
  ( Price(..)
  ) where

import Data.Commodity (CommodityName)

data Price a = Price
  { _amount :: a
  , _targetCommodity :: CommodityName
  , _sourceCommodity :: CommodityName
  } deriving (Eq, Show, Functor)
