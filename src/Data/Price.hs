module Data.Price
  ( Price(..)
  , (<@>)
  ) where

import Data.Amount (Amount(..))
import Data.Commodity (CommodityName)

data Price a = Price
  { _sourceCommodity :: CommodityName
  , _amount :: Amount a
  } deriving (Eq, Show, Functor)

(<@>) :: Num a => Amount a -> Price a -> Amount a
(Amount a c) <@> (Price psc pa)
  | psc == c = (* a) <$> pa
  | otherwise = error "Source commodity must match with amount commodity"
