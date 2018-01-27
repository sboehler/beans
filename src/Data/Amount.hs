module Data.Amount
  ( Amount(..)
  , (<@@>)
  ) where

import Data.Commodity (CommodityName)

data Amount a = Amount
  { _amount :: a
  , _commodity :: CommodityName
  } deriving (Eq, Show, Functor)

(<@@>) :: Num a => Amount a -> Amount a -> Amount a
(Amount a _) <@@> amount = (signum a *) <$> amount
