module Data.Price
  ( Price(..)
  , (<@>)
  ) where

import Data.Amount (Amount(..))
import Data.Commodity (CommodityName)
import Data.Text.Prettyprint.Doc (Pretty, pretty)

data Price a = Price
  { _sourceCommodity :: CommodityName
  , _amount :: Amount a
  } deriving (Eq, Show, Functor)

instance Show a =>
         Pretty (Price a) where
  pretty Price {..} = pretty _amount

(<@>)
  :: Num a
  => Amount a -> Price a -> Amount a
(Amount a c) <@> (Price psc pa)
  | psc == c = (* a) <$> pa
  | otherwise = error "Source commodity must match with amount commodity"
